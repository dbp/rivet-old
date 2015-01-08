#!/bin/bash

BACKUP=/backups

pg_dump {{project_slug}}_prod > $BACKUP/dump.txt

/usr/local/bin/tarsnap --cachedir /usr/tarsnap-cache --keyfile {{ base_dir }}/tarsnap.key -c -f {{project_slug}}-`date +%m.%d.%Y` $BACKUP && curl https://nosnch.in/2c7d6c71c4 &> /dev/null
