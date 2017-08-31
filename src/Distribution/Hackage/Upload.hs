{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Distribution.Hackage.Upload
       ( HackageSettings(..)
       , UploadStatus(..)
       , PackageName
       , Uploader(..)
       , buildAndUploadPackage
       , hackageUpload
       , uploadDocs) where

import Prelude hiding (FilePath)
import System.Directory
import Shelly
import Data.String
import System.Info
import Text.Printf
import qualified Data.Text as T
import Data.Monoid


--------------------------------------------------------------------------------
type PackageName = String

--------------------------------------------------------------------------------
data Uploader =
    UPL_cabal
  | UPL_stack
  deriving (Show, Eq, Enum, Bounded)

--------------------------------------------------------------------------------
data HackageSettings = HackageSettings {
    hackageUrl  :: !T.Text
  , hackageUser :: !T.Text
  , hackageWhiteList :: [T.Text]
  , hackagePrivatePackage :: !Bool
  , hackagePwd  :: !T.Text
  , hackageUploader :: Uploader
  , hackageBuildDocs :: !Bool
  , hackagePackageName :: PackageName
  , hackagePackageVersion :: !T.Text
  } deriving Show


--------------------------------------------------------------------------------
data UploadStatus = Uploaded | Skipped deriving Show


--------------------------------------------------------------------------------
hackageUpload :: HackageSettings -> IO ()
hackageUpload settings@HackageSettings{..} = shelly $ verbosely $ do
  status <- buildAndUploadPackage settings
  when hackageBuildDocs $ uploadDocs status settings

--------------------------------------------------------------------------------
cabal :: [T.Text] -> Sh ()
cabal = command_ "cabal" []

--------------------------------------------------------------------------------
stack :: [T.Text] -> Sh ()
stack = command_ "stack" []

--------------------------------------------------------------------------------
htmlLocation :: HackageSettings -> T.Text
htmlLocation HackageSettings{..} =
  T.pack "--html-location=\"" <>
  hackageUrl <>
  "/package/" <> T.pack hackagePackageName <> "-" <>
  hackagePackageVersion <> "/docs\""


--------------------------------------------------------------------------------
contentsLocation :: HackageSettings -> T.Text
contentsLocation HackageSettings{..} =
  T.pack "--contents-location=\"" <>
  hackageUrl <>
  "/package/" <> T.pack hackagePackageName <> "-" <>
  hackagePackageVersion <> "\""


--------------------------------------------------------------------------------
-- Strip the dev flags so that the package would pass `cabal sdist`.
-- For stack we don't call this as those should be outsourced into the
-- `stack.yaml` file. Exception has to be made for flags controlled by a flag,
-- which apparently gets rejected nevertheless..
stripDevFlags :: Uploader -> HackageSettings -> Sh ()
stripDevFlags UPL_stack HackageSettings{..} = do
  echo $ "* Not stripping dev flags as stack has been detected as the uploader.\n" <>
         "Please consider outsourcing flags like -Wall or -Werror into the stack.yaml manifest."
  let manifest = T.pack hackagePackageName <> ".cabal"
  let bakFile  = manifest <> ".bak"
  stripPerfAndTestFlags manifest bakFile
stripDevFlags UPL_cabal HackageSettings{..} = do
  echo "Stripping dev flags (if needed) ..."
  let manifest = T.pack hackagePackageName <> ".cabal"
  let bakFile  = manifest <> ".bak"
  stripCompilationFlags manifest bakFile
  stripPerfAndTestFlags manifest bakFile

--------------------------------------------------------------------------------
-- | Strip things like "-Wall" and "-Werror" from the manifest.
stripCompilationFlags :: T.Text -> T.Text -> Sh ()
stripCompilationFlags manifest bakFile = do
  let removeWError = T.pack "/.*-Werror.*$/d"
  run_ "sed" ["-i.bak", removeWError, manifest]
  run_ "rm" ["-rf", bakFile]

--------------------------------------------------------------------------------
-- | Strip things like "-fhpc".
stripPerfAndTestFlags :: T.Text -> T.Text -> Sh ()
stripPerfAndTestFlags manifest bakFile = do
  let removeHpcIf = T.pack "/.*if.*flag.*(hpc).*$/d"
  let removeHpc = T.pack "/.*ghc-options:.*-fhpc.*$/d"
  run_ "sed" ["-i.bak", removeHpcIf, manifest]
  run_ "rm" ["-rf", bakFile]
  run_ "sed" ["-i.bak", removeHpc, manifest]
  run_ "rm" ["-rf", bakFile]

--------------------------------------------------------------------------------
alreadyUploaded :: HackageSettings -> Sh Bool
alreadyUploaded HackageSettings{..} = do
  resp <- run "curl" ["-I", hackageUri]
  echo resp
  return $ "200" `T.isInfixOf` resp
  where
    hackageUri = T.pack $
      printf "%s/package/%s-%s"
             (T.unpack hackageUrl)
             hackagePackageName
             (T.unpack hackagePackageVersion)

--------------------------------------------------------------------------------
toUploader :: Uploader -> [T.Text] -> Sh ()
toUploader UPL_cabal = cabal
toUploader UPL_stack = stack

--------------------------------------------------------------------------------
buildAndUploadPackage :: HackageSettings -> Sh UploadStatus
buildAndUploadPackage settings@HackageSettings{..} = do
  skipIt <- alreadyUploaded settings
  if skipIt
    then return Skipped
    else do
      let uploader = toUploader hackageUploader
      when (hackageUploader == UPL_cabal) $ uploader ["configure"]
      uploader ["build"]
      uploader ["sdist"]
      let fileName = fromString (hackagePackageName
                                 <> "-"
                                 <> T.unpack hackagePackageVersion)
      ddir <- distDir hackageUploader
      chdir ddir $ do
        run_ "rm"  ["-rf", toTextIgnore fileName]
        run_ "tar" ["-xzf", toTextIgnore fileName <> ".tar.gz"]
        run_ "rm"  ["-rf", toTextIgnore fileName <> ".tar.gz"]
        chdir fileName (stripDevFlags hackageUploader settings)
        run_ "tar" (extraTarFlags <> ["-czf", toTextIgnore fileName <> ".tar.gz", toTextIgnore fileName])
        echo "Uploading package to Hackage..."
        -- Use cabal for the final upload step. This is necessary
        -- as stack does not allow you to specify things like user/pwd etc.
        case (hackageUrl == "hackage.haskell.org" && hackagePrivatePackage ||
             (hackageUrl `notElem` hackageWhiteList) && hackagePrivatePackage) of
          True -> error "Cowardly refusing to upload: This package is marked as private."
          False -> do
            cabal ["upload", "-v3", "-u", hackageUser, "-p", hackagePwd, tarball fileName]
        return Uploaded

  where
    tarball fl = toTextIgnore fl <> ".tar.gz"

--------------------------------------------------------------------------------
extraTarFlags :: [T.Text]
extraTarFlags = case System.Info.os of
  "linux"  -> ["--format=ustar"]
  _        -> mempty

--------------------------------------------------------------------------------
distDir :: Uploader -> Sh FilePath
distDir UPL_cabal = return "dist"
distDir UPL_stack = escaping False (fromText . T.strip <$> run "stack" ["path", "--dist-dir"])

--------------------------------------------------------------------------------
uploadDocs :: UploadStatus -> HackageSettings -> Sh ()
uploadDocs status settings@HackageSettings{..} =
  case status of
    Skipped -> do
      cabal ["configure"]
      upload_
    _ -> upload_
  where
    upload_ = do
        let docsFilename = T.pack $ printf "%s-%s-docs"
                                    hackagePackageName
                                    (T.unpack hackagePackageVersion)
        let docsTarball = docsFilename <> ".tar.gz"
        alreadyGenerated <- liftIO (doesFileExist . T.unpack $ "dist/doc/html/" <> docsTarball)
        unless alreadyGenerated $ errExit False $ do
          cabal ["haddock", "--hyperlink-source",
                 htmlLocation settings,
                 contentsLocation settings, "--executables"]
          out <- lastStderr
          let failedForTransitivity   = "transitive" `T.isInfixOf` out
          when failedForTransitivity $
            -- back off and build only libraries
            cabal ["haddock", "--hyperlink-source",
                   htmlLocation settings,
                   contentsLocation settings]
        run_ "mkdir" ["-p", "dist/doc/html"]
        chdir "dist/doc/html" $ do
          run_ "cp"  ["-r", T.pack hackagePackageName, docsFilename]
          run_ "gtar" ["-cvz", "-Hustar",
                      "-f", docsTarball, docsFilename]
          echo "Uploading docs to Hackage..."
          run_ "curl" ["-X", "PUT",
                       "-H", "Content-Type: application/x-tar",
                       "-H", "Content-Encoding: gzip",
                       "--data-binary", "@" <> docsTarball,
                       hackageUploadUrl]
    hackageUploadUrl = T.pack $
      printf "http://%s:%s@%s/package/%s-%s/docs"
             (T.unpack hackageUser)
             (T.unpack hackagePwd)
             (T.unpack $ T.replace "http://" "" hackageUrl)
             hackagePackageName
             (T.unpack hackagePackageVersion)
