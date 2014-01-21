{-# LANGUAGE OverloadedStrings #-}
module Main where

import Distribution.Hackage.Upload
import System.Environment
import qualified Data.Text as T

main :: IO ()
main = do
   args <- getArgs
   case args of
     [url, pkg, ver, usr, pwd ] ->
       let settings = HackageSettings {
            hackageUrl = T.pack url
          , hackageUser = T.pack usr
          , hackagePwd  = T.pack pwd
          , hackagePackageName = pkg
          , hackagePackageVersion = T.pack ver
       } in hackageUpload settings
     [pkg, ver, usr, pwd ] ->
       let settings = HackageSettings {
            hackageUrl = T.pack "http://hackage.haskell.org"
          , hackageUser = T.pack usr
          , hackagePwd  = T.pack pwd
          , hackagePackageName = pkg
          , hackagePackageVersion = T.pack ver
       } in hackageUpload settings
     _ -> print "Usage: [url] pkgName version username pwd"
