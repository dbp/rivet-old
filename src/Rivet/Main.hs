module Main where

import           Control.Applicative     ((<$>))
import           Control.Monad           (void, when)
import           Data.Char               (isSpace)
import           Data.Configurator
import           Data.Configurator.Types
import qualified Data.HashMap.Strict     as M
import           Data.List               (intercalate, isInfixOf)
import           Data.Monoid
import qualified Data.Text               as T
import           Data.Time.Clock
import           Data.Time.Format
import           Development.Shake
import           Prelude                 hiding ((++))
import           System.Directory        (createDirectoryIfMissing,
                                          getCurrentDirectory)
import           System.Exit
import           System.IO
import           System.Process

import           Rivet.Common
import qualified Rivet.Rules             as Rules
import qualified Rivet.Tasks             as Tasks

opts :: ShakeOptions
opts = shakeOptions { shakeFiles    = ".shake/" }

getProjectName :: IO String
getProjectName = (reverse . takeWhile (/= '/') . reverse) <$>
                   getCurrentDirectory

main :: IO ()
main = do
  proj <- getProjectName
  conf <- load [Required "Rivetfile"]
  commands <- (map (\(k,String v) -> (T.drop (length "commands.") k, v)) .
               filter (\(k,v) -> (T.pack "commands.") `T.isPrefixOf` k) .
               M.toList) <$> getMap conf
  deps <- lookupDefault [] conf (T.pack "dependencies")
  shakeArgsWith opts [] $ \flags targets -> return $ Just $ do
     case targets of
       [] -> action $ liftIO $ putStrLn "Need a task. Run `rivet tasks` to see all tasks."
       ("test":_) -> want ["test"]
       ("db:new":_:[]) -> want ["db:new"]
       ("db:new":_) -> action $ liftIO (putStrLn "usage: rivet db:new migration_name")
       _ -> want targets

     Rules.addCommands commands
     Rules.addDependencies deps
     Rules.addBinary proj
     "cabal.sandbox.config" *> \_ -> cmd "cabal sandbox init"
     "run" ~> Tasks.run proj
     "run:docker" ~> Tasks.runDocker proj
     "test" ~> Tasks.test targets
     "db" ~> Tasks.db proj conf
     "db:test" ~> Tasks.dbTest proj conf
     "db:create" ~> Tasks.dbCreate proj conf
     "db:new" ~> Tasks.dbNew targets
     "db:migrate" ~> Tasks.dbMigrate
     "db:migrate:down" ~> Tasks.dbMigrateDown
     "db:status" ~> Tasks.dbStatus
     "db:migrate:docker" ~> Tasks.dbMigrateDocker proj
     "db:migrate:down:docker" ~> Tasks.dbMigrateDownDocker proj
     "db:status:docker" ~> Tasks.dbStatusDocker proj
     "repl" ~> Tasks.repl
     "setup" ~> Tasks.setup
     "deploy:status" ~> Tasks.deployStatus proj conf
     "deploy:migrate" ~> Tasks.deployMigrate proj conf
     "deploy:rollout" ~> Tasks.deployRollout proj conf
     "crypt:edit" ~> Tasks.cryptEdit proj
     "crypt:show" ~> Tasks.cryptShow
     "crypt:setpass" ~> Tasks.cryptSetPass proj

     "tasks" ~> liftIO (mapM_ (putStrLn . ("rivet " ++))
                              [ "run"
                              , "run:docker"
                              ,"test [pattern]"
                              ,"db"
                              ,"db:create"
                              ,"db:new migration_name"
                              ,"db:migrate"
                              ,"db:migrate:down"
                              ,"db:status"
                              ,"db:migrate:docker"
                              ,"db:migrate:down:docker"
                              ,"db:status:docker"
                              ,"repl"
                              ,"setup"
                              ,"deploy:status"
                              ,"deploy:migrate"
                              ,"deploy:rollout"
                              ,"crypt:edit"
                              ,"crypt:show"
                              ,"crypt:setpass"
                              ])
