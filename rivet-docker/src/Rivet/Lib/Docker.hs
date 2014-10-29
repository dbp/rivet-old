module Rivet.Lib.Docker where

import           Prelude       hiding ((++))

import           Control.Monad (void)
import           Rivet.Common

tasks :: [Task]
tasks = [Task "run:docker" 0 runDocker ""
        ,Task "db:docker" 0 dbDocker ""
        ,Task "db:create:docker" 0 dbCreateDocker ""
        ,Task "db:migrate:docker" 0 dbMigrateDocker ""
        ,Task "db:migrate:down:docker" 0 dbMigrateDownDocker ""
        ,Task "db:status:docker" 0 dbStatusDocker ""
        ]

cmdDocker proj command =
  do exec "ln -sf docker/Dockerfile.development Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_devel ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /" ++ proj ++ " -i -t -v $PWD/migrations:/" ++ proj ++ "/migrations -v $PWD/docker/data:/var/lib/postgresql -e CMD=\"" ++ command ++ "\" "
                   ++ proj ++ "_devel /" ++ proj ++ "/run"

-- NOTE(dbp 2014-10-29): Have directories we want to bind be in
-- Rivetfile... In particular, templates and emails are not general...
runDocker proj _ _ =
  do exec "ln -sf docker/Dockerfile.development Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_devel ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /" ++ proj ++ " -p 8000:8000 -i -t -v $PWD/docker/data:/var/lib/postgresql -v $PWD/migrations:/" ++ proj ++ "/migrations -v $PWD/snaplets:/" ++ proj ++ "/snaplets -v $PWD/templates:/" ++ proj ++ "/templates -v $PWD/emails:/" ++ proj ++ "/emails -v $PWD/static:/" ++ proj ++ "/static -v $PWD/src:/" ++ proj ++ "/src -v $PWD/devel.cfg:/" ++ proj ++ "/devel.cfg -v $PWD/defaults.cfg:/" ++ proj ++ "/defaults.cfg -e CMD=\"rivet run\" " ++ proj ++ "_devel /" ++ proj ++ "/run"

dbDocker proj _ _ = cmdDocker proj "rivet db"

dbCreateDocker proj _ _ = cmdDocker proj "rivet db:create"

dbMigrateDocker proj _ _ = cmdDocker proj "rivet db:migrate"

dbMigrateDownDocker proj _ _ = cmdDocker proj "rivet db:migrate:down"

dbStatusDocker proj _ _ = cmdDocker proj "rivet db:status"
