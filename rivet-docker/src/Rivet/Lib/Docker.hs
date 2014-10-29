module Rivet.Lib.Docker where

import           Prelude       hiding ((++))

import           Control.Monad (void)
import           Rivet.Common

tasks :: [Task]
tasks = [Task "run:docker" 0 runDocker ""
        ,Task "db:migrate:docker" 0 dbMigrateDocker ""
        ,Task "db:migrate:down:docker" 0 dbMigrateDownDocker ""
        ,Task "db:status:docker" 0 dbStatusDocker ""
        ]

-- NOTE(dbp 2014-10-29): Have directories we want to bind be in
-- Rivetfile... In particular, templates and emails are not general...
runDocker proj _ _ =
  do exec "ln -sf docker/Dockerfile.development Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_devel ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -p 8000:8000 -i -t -v $PWD/docker/data:/var/lib/postgresql -v $PWD/snaplets:/srv/snaplets -v $PWD/templates:/srv/templates -v $PWD/emails:/srv/emails -v $PWD/static:/srv/static -v $PWD/src:/srv/src -v $PWD/devel.cfg:/srv/devel.cfg -v $PWD/defaults.cfg:/srv/defaults.cfg -e CMD=\"rivet run\" " ++ proj ++ "_devel /srv/run"

dbMigrateDocker proj _ _ =
  do exec "ln -sf docker/Dockerfile.development Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_devel ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -i -t -v $PWD/migrations:/srv/migrations -v $PWD/docker/data:/var/lib/postgresql -e CMD=\"rivet db:status\" "
                   ++ proj ++ "_devel /srv/run"

dbMigrateDownDocker proj _ _ =
  do exec "ln -sf docker/Dockerfile.development Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_devel ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -i -t -v $PWD/migrations:/srv/migrations -v $PWD/docker/data:/var/lib/postgresql -e CMD=\"rivet db:status\" "
                   ++ proj ++ "_devel /srv/run"

dbStatusDocker proj _ _ =
  do exec "ln -sf docker/Dockerfile.development Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_devel ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -i -t -v $PWD/migrations:/srv/migrations -v $PWD/docker/data:/var/lib/postgresql -e CMD=\"rivet db:status\" "
                   ++ proj ++ "_devel /srv/run"
