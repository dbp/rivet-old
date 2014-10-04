{-# LANGUAGE TemplateHaskell #-}
module Rivet.TH where

import qualified Data.Foldable              as F
import           Data.List
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.Directory.Tree
import           System.FilePath

type FileData = (String, String)
type DirData = FilePath

loadFile :: String -> FilePath -> Q [Dec]
loadFile nm pth = do let ident = mkName nm
                     typeSig <- SigD ident `fmap` [t| String |]
                     v <- valD (varP ident) (normalB $ lift =<< runIO (readFile pth)) []
                     return [typeSig, v]

loadProjectTemplate :: Q [Dec]
loadProjectTemplate = do let dir = mkName "tDirTemplate"
                         typeSig <- SigD dir `fmap` [t| ([String], [(String, String)]) |]
                         v <- valD (varP dir) (normalB $ dirQ ("template" </> "project")) []
                         return [typeSig, v]

loadModelTemplate :: Q [Dec]
loadModelTemplate = do let dir = mkName "tModelTemplate"
                       typeSig <- SigD dir `fmap` [t| ([String], [(String, String)]) |]
                       v <- valD (varP dir) (normalB $ dirQ ("template" </> "model")) []
                       return [typeSig, v]

-- NOTE(dbp 2014-09-27): Much of this code is derived from that used
-- in the Snap project starter.
------------------------------------------------------------------------------
-- Gets all the directories in a DirTree
--
getDirs :: [FilePath] -> DirTree a -> [FilePath]
getDirs prefix (Dir n c) = (intercalate "/" (reverse (n:prefix))) :
                           concatMap (getDirs (n:prefix)) c
getDirs _ (File _ _) = []
getDirs _ (Failed _ _) = []

------------------------------------------------------------------------------
-- Reads a directory and returns a tuple of the list of all directories
-- encountered and a list of filenames and content strings.
--
readTree :: FilePath -> IO ([DirData], [FileData])
readTree dir = do d <- readDirectory $ dir </> "."
                  let ps = zipPaths $ "" :/ (dirTree d)
                      fd = F.foldr (:) [] ps
                      dirs = getDirs [] $ dirTree d
                  return (drop 1 dirs, fd)
------------------------------------------------------------------------------
-- Calls readTree and returns its value in a quasiquote.
--
dirQ :: FilePath -> Q Exp
dirQ tplDir = do d <- runIO . readTree $ tplDir
                 lift d
