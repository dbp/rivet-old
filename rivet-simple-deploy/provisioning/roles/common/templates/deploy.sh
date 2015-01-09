#!/bin/bash

set -eu

source docker_credentials.sh

if [ "$#" -ne 5 ]
then
    echo "usage : deploy.sh repo env imgname SHA 3 -- start three new repo/env instances of imgname:SHA"
    exit -1
fi

REPO=$1
ENV=$2
SLUG=${REPO}_${ENV}
IMG=$3
SHA=$4
NUM=$5
CFG=prod_${SHA}.cfg

if [ -f "/srv/env_${ENV}" ]
then
    ENV_FILE="/srv/env_${ENV}"
else
    ENV_FILE="/srv/env"
fi

if [ ! -f ${ENV_FILE} ]
then
    echo "**** No /srv/env or /srv/env_${ENV} file found. Aborting."
fi

echo "------> Logging in to Docker Hub..."
docker login -e $DOCKER_EMAIL -u $DOCKER_USERNAME -p $DOCKER_PASSWORD &> /dev/null

echo "------> Getting currently running containers..."
OLDPORTS=( `docker ps | grep ${SLUG}_ | awk '{print $1}'` )
declare -A OLDUPSTREAM
for up in `etcdctl ls ${REPO}/${ENV}/upstream`
do
    OLDUPSTREAM["$up"]="$up"
done

echo "        Found ${#OLDPORTS[@]} running containers: ${OLDPORTS[@]}."

echo "------> Pulling $IMG:$SHA..."
docker pull $IMG:$SHA &> /dev/null

echo "------> Starting $NUM new containers..."
for i in `seq 1 $NUM` ; do
    UNIQ=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 8 | head -n 1 | tr '[:upper:]' '[:lower:]')
    JOB=`docker run -d -w /srv -p 8000 --env-file=${ENV_FILE} -v /srv/data:/srv/data -v /var/run/redis/redis.sock:/tmp/redis.sock --name=${SLUG}_${UNIQ} ${IMG}:${SHA} | cut -c1-12`
    if [ -z "$JOB" ]
    then
	echo "**** Could not create new container. Aborting."
	exit -1
    fi
    PORT=`docker inspect $JOB | grep HostPort | cut -d '"' -f 4 | grep -v '^$'`
    if [ -z "$PORT" ]
    then
	echo "**** Could not find PORT for container. Aborting."
	exit -1
    fi
    echo "        Waiting for new container $JOB to boot (GET /_healthcheck)..."
    RES=`wget --retry-connrefused --waitretry=1 --read-timeout=20 --timeout=15 -t 5 -q -S -O - 127.0.0.1:$PORT/_healthcheck 2>&1 | grep "200 OK"`
    if [ -z "$RES" ]
    then
        echo "**** Container failed to boot. Aborting."
	exit -1
    fi
    etcdctl set "${REPO}/${ENV}/upstream/${JOB}" "127.0.0.1:$PORT" > /dev/null
done

echo "------> Shutting down old containers..."

for i in ${OLDPORTS[@]}
do
    echo "        Stopping container $i..."
    if [ ${OLDUPSTREAM["/${REPO}/${ENV}/upstream/$i"]+_} ]
    then
        unset OLDUPSTREAM["/${REPO}/${ENV}/upstream/$i"]
    fi
    # NOTE(dbp 2015-01-08): If a container is running that isn't in etcd,
    # we just want it killed, so we don't care if this rm fails.
    etcdctl rm /${REPO}/${ENV}/upstream/$i || true &> /dev/null
    sudo /usr/local/bin/confd -onetime -quiet
    sleep 5
    docker kill $i > /dev/null
done

# NOTE(dbp 2015-01-08): These are dead and gone containers.
for i in ${!OLDUPSTREAM[@]}
do
    etcdctl rm $i
done

sudo /usr/local/bin/confd -onetime -quiet

echo "------> Deploy complete."
