module Rivet.Lib.SimpleDeploy where

import           Prelude                 hiding ((++))

import           Control.Monad           (void)
import           Control.Monad.Trans     (liftIO)
import           Data.Configurator
import           Data.Configurator.Types
import qualified Data.Text               as T
import           Rivet.Common

tasks :: [Task]
tasks = [Task "deploy:status" 0 deployStatus ""
        ,Task "deploy:migrate" 0 deployMigrate ""
        ,Task "deploy:migrate:status" 0 deployMigrateStatus ""
        ,Task "deploy:migrate:down" 0 deployMigrateDown ""
        ,Task "deploy:rollout" 0 deployRollout ""
        ,Task "deploy:rollback" 1 deployRollback "SHA (short)"
        ]


deployMigrate proj conf _ =
  do stageHost <- liftIO $ require conf (T.pack "stage-host")
     prodImage <- liftIO $ require conf (T.pack "production-image")
     tag <- getDockerTag proj stageHost "stage"
     if length tag < 5
        then liftIO $ putStrLn "Couldn't get tag from staging."
        else do let c = "docker run -w /srv -i -t -v /srv/data:/srv/data -v /var/run/postgresql/.s.PGSQL.5432:/var/run/postgresql/.s.PGSQL.5432 -v /srv/prod_" ++ tag ++ ".cfg:/srv/prod.cfg " ++ prodImage ++ ":" ++ tag ++ " rivet db:migrate prod"
                void $ exec $ "ssh " ++ stageHost ++ " " ++ c

deployMigrateStatus proj conf _ =
  do stageHost <- liftIO $ require conf (T.pack "stage-host")
     prodImage <- liftIO $ require conf (T.pack "production-image")
     tag <- getDockerTag proj stageHost "stage"
     if length tag < 5
        then liftIO $ putStrLn "Couldn't get tag from staging."
        else do let c = "docker run -w /srv -i -t -v /srv/data:/srv/data -v /var/run/postgresql/.s.PGSQL.5432:/var/run/postgresql/.s.PGSQL.5432 -v /srv/prod_" ++ tag ++ ".cfg:/srv/prod.cfg " ++ prodImage ++ ":" ++ tag ++ " rivet db:status prod"
                void $ exec $ "ssh " ++ stageHost ++ " " ++ c

deployMigrateDown proj conf _ =
  do stageHost <- liftIO $ require conf (T.pack "stage-host")
     prodImage <- liftIO $ require conf (T.pack "production-image")
     tag <- getDockerTag proj stageHost "stage"
     if length tag < 5
        then liftIO $ putStrLn "Couldn't get tag from staging."
        else do let c = "docker run -w /srv -i -t -v /srv/data:/srv/data -v /var/run/postgresql/.s.PGSQL.5432:/var/run/postgresql/.s.PGSQL.5432 -v /srv/prod_" ++ tag ++ ".cfg:/srv/prod.cfg " ++ prodImage ++ ":" ++ tag ++ " rivet db:migrate:down prod"
                void $ exec $ "ssh " ++ stageHost ++ " " ++ c

deployStatus proj conf _ =
  do stageHost <- liftIO $ require conf (T.pack "stage-host")
     prodHost <- liftIO $ require conf (T.pack "prod-host")
     stageTag <- getDockerTag proj stageHost "stage"
     prodTag <- getDockerTag proj prodHost "prod"
     if length stageTag < 5
        then liftIO $ putStrLn "Couldn't get staging tag."
        else do liftIO $ putStrLn "Staging is running..."
                void $ exec $ "git rev-list --format=%B --max-count=1 " ++ stageTag
     if length prodTag < 5
        then liftIO $ putStrLn "Couldn't get production tag."
        else do liftIO $ putStrLn "Production is running..."
                void $ exec $ "git rev-list --format=%B --max-count=1 " ++ prodTag

deployRollout proj conf _ =
  do stageHost <- liftIO $ require conf (T.pack "stage-host")
     prodHost <- liftIO $ require conf (T.pack "prod-host")
     prodImage <- liftIO $ require conf (T.pack "production-image")
     prodInstances <- liftIO $ lookupDefault (1 :: Int) conf (T.pack "production-instances")
     tag <- getDockerTag proj stageHost "stage"
     if length tag < 5
        then liftIO $ putStrLn "Couldn't get tag from staging."
        else do liftIO $ putStrLn "Deploying..."
                exec $ "git rev-list --format=%B --max-count=1 " ++ tag
                void $ exec $ "ssh " ++ prodHost ++ " /srv/deploy.sh " ++ proj ++ " prod " ++ prodImage ++ " " ++ tag ++ " " ++ (show prodInstances)

deployRollback proj conf (tag:_) =
  do prodHost <- liftIO $ require conf (T.pack "prod-host")
     prodImage <- liftIO $ require conf (T.pack "production-image")
     prodInstances <- liftIO $ lookupDefault (1 :: Int) conf (T.pack "production-instances")
     liftIO $ putStrLn $ "Rolling back to " ++ tag ++ "..."
     exec $ "git rev-list --format=%B --max-count=1 " ++ tag
     void $ exec $ "ssh " ++ prodHost ++ " /srv/deploy.sh " ++ proj ++ " prod " ++ prodImage ++ " " ++ tag ++ " " ++ (show prodInstances)
