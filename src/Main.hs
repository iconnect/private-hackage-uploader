{-# LANGUAGE OverloadedStrings #-}
module Main where

import Distribution.Hackage.Upload
import System.Environment
import Control.Monad
import qualified Data.Text as T

main :: IO ()
main = do
   args <- getArgs
   case args of
     [url, pkg, ver, usr, pwd ] ->
       let settings = HackageSettings {
            hackageUrl = T.pack url
          , hackageUser = T.pack usr
          , hackageWhiteList = [T.pack url]
          , hackagePwd  = T.pack pwd
          , hackageUploader = UPL_cabal
          , hackagePackageName = pkg
          , hackageBuildDocs = True
          , hackagePrivatePackage = False
          , hackagePackageVersion = T.pack ver
       } in hackageUpload settings
     [pkg, ver, usr, pwd ] -> do
       putStrLn "Going to upload to main Hackage. ARE YOU SURE??? [y/n]"
       areYouSure <- getLine
       putStrLn "Are you REALLY sure??? [y/n]"
       reallySure <- getLine
       when (yesNoParse [areYouSure, reallySure]) $ do
         let settings = HackageSettings {
              hackageUrl = T.pack "http://hackage.haskell.org"
            , hackageUser = T.pack usr
            , hackageUploader = UPL_cabal
            , hackageWhiteList = ["http://hackage.haskell.org"]
            , hackagePwd  = T.pack pwd
            , hackagePackageName = pkg
            , hackageBuildDocs = True
            , hackagePrivatePackage = False
            , hackagePackageVersion = T.pack ver
         }
         hackageUpload settings
     _ -> putStrLn "Usage: [url] pkgName version username pwd"
  where
   yesNoParse :: [String] -> Bool
   yesNoParse = all parseIt

   parseIt "y" = True
   parseIt "Y" = True
   parseIt _   = False
