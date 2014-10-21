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


runDocker proj _ _ =
  do exec "ln -sf docker/Dockerfile.development Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_devel ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -p 8000:8000 -i -t -v $PWD/docker/data:/var/lib/postgresql -v $PWD/snaplets:/srv/snaplets -v $PWD/static:/srv/static -v $PWD/src:/srv/src -v $PWD/devel.cfg:/srv/devel.cfg -v $PWD/defaults.cfg:/srv/defaults.cfg " ++ proj ++ "_devel"

dbMigrateDocker proj _ _ =
  do exec "ln -sf docker/Dockerfile.migrate Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_migrate ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -i -t -e \"MODE=up\" -v $PWD/docker/data:/var/lib/postgresql "
                   ++ proj ++ "_migrate"

dbMigrateDownDocker proj _ _ =
  do exec "ln -sf docker/Dockerfile.migrate Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_migrate ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -i -t -e \"MODE=down\" -v $PWD/docker/data:/var/lib/postgresql "
                    ++ proj ++ "_migrate"

dbStatusDocker proj _ _ =
  do exec "ln -sf docker/Dockerfile.migrate Dockerfile"
     exec $ "sudo docker build -t " ++ proj ++ "_migrate ."
     exec "rm Dockerfile"
     void $ exec $ "sudo docker run -w /srv -i -t -e \"MODE=status\" -v $PWD/docker/data:/var/lib/postgresql "
                   ++ proj ++ "_migrate"
