module Rivet.Main where

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
import           Development.Shake       hiding (doesFileExist)
import           Prelude                 hiding ((++))
import           System.Directory        (createDirectoryIfMissing,
                                          doesFileExist, getCurrentDirectory)
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

mainWith :: [Task] -> IO ()
mainWith tasks = do
  proj <- getProjectName
  shakeArgsWith opts [] $ \flags targets ->
    case targets of
      ("init":[]) -> do
        e <- doesFileExist "Rivetfile"
        if e
           then
             do putStrLn "Error: Rivetfile already exists. Only run 'rivet init' in empty directory."
                return Nothing
           else return $ Just $ do want ["init"]
                                   "init" ~> Tasks.init proj
      ("init":_) -> do putStrLn "Usage: rivet init"
                       return Nothing
      _ -> do conf <- load [Required "Rivetfile"]
              commands <- (map (\(k,String v) -> (T.drop (length "commands.") k, v)) .
                           filter (\(k,v) -> (T.pack "commands.") `T.isPrefixOf` k) .
                           M.toList) <$> getMap conf
              deps <- lookupDefault [] conf (T.pack "dependencies")
              cabal <- lookupDefault "cabal " conf (T.pack "cabal-command")
              return $ Just $ do
                case targets of
                  [] -> action $ liftIO $ putStrLn "Need a task. Run `rivet tasks` to see all tasks."
                  ("test":_) -> want ["test"]
                  ("db:new":_:[]) -> want ["db:new"]
                  ("db:new":_) -> action $ liftIO (putStrLn "usage: rivet db:new migration_name")
                  ("model:new":[]) -> action $ liftIO (putStrLn "usage: rivet model:new ModelName [field_name:field_type]*")
                  ("model:new":_) -> want ["model:new"]
                  ("db:migrate":_:[]) -> want ["db:migrate"]
                  ("db:migrate":_:_) -> action $ liftIO (putStrLn "usage: rivet db:migrate [env]")
                  ("db:migrate:down":_:[]) -> want ["db:migrate:down"]
                  ("db:migrate:down":_:_) -> action $ liftIO (putStrLn "usage: rivet db:migrate:down [env]")
                  ("db:status":_:[]) -> want ["db:status"]
                  ("db:status":_:_) -> action $ liftIO (putStrLn "usage: rivet db:status [env]")
                  (target:args) ->
                    do mapM_ (\t ->
                             if taskName t == target
                                then if length args == taskNumArgs t
                                        then want [taskName t]
                                        else action $ liftIO (putStrLn $
                                                                "usage: rivet " ++
                                                                taskName t ++ " " ++
                                                                taskUsage t)
                                else return ())
                             tasks
                       when (not (target `elem` (map taskName tasks))) $ want targets
                Rules.addCommands commands
                Rules.addDependencies cabal deps
                Rules.addBinary cabal proj
                mapM_ (\t -> taskName t ~> taskBody t proj conf (tail targets)) tasks
                "cabal.sandbox.config" *> \_ -> cmd "cabal sandbox init"
                "run" ~> Tasks.run proj
                "test" ~> Tasks.test cabal targets
                "db" ~> Tasks.db proj conf
                "db:test" ~> Tasks.dbTest proj conf
                "db:create" ~> Tasks.dbCreate proj conf
                "db:new" ~> Tasks.dbNew targets
                "db:migrate" ~> Tasks.dbMigrate cabal proj conf (tail targets)
                "db:migrate:down" ~> Tasks.dbMigrateDown cabal proj conf (tail targets)
                "db:status" ~> Tasks.dbStatus cabal proj conf (tail targets)
                "model:new" ~> Tasks.modelNew proj targets
                "repl" ~> Tasks.repl cabal
                "setup" ~> Tasks.setup cabal
                "crypt:edit" ~> Tasks.cryptEdit proj
                "crypt:show" ~> Tasks.cryptShow
                "crypt:setpass" ~> Tasks.cryptSetPass proj

                "tasks" ~> liftIO (mapM_ (putStrLn . ("rivet " ++)) $
                                       ["init"
                                       ,"run"
                                       ,"test [pattern]"
                                       ,"db"
                                       ,"db:test"
                                       ,"db:create"
                                       ,"db:new migration_name"
                                       ,"db:migrate"
                                       ,"db:migrate:down"
                                       ,"db:status"
                                       ,"model:new ModelName"
                                       ,"repl"
                                       ,"setup"
                                       ] ++ map taskName tasks)
