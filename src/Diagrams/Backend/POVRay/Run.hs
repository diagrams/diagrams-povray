{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.POVRay.Run
-- Copyright   :  (c) 2018 Diagrams-povray team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- An experimental backend for three-dimensional diagrams.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.POVRay.Run
  ( PovOpts (..)
  , runPovray
  ) where

import System.Process
import System.Exit
import System.IO.Temp

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.FilePath

data PovOpts = PovOpts

povrayPath :: FilePath
povrayPath = "povray"

runPovray
  :: PovOpts
  -> String
   -- ^ povray source
  -> IO (String, Either String ByteString)
  -- ^ stdout (either stderr or result)
runPovray opts source =
  withSystemTempDirectory "povray" $ \tempDir -> do
    let sourceFile = tempDir </> "source.pov"
    writeFile sourceFile source
    let resultFile = tempDir </> "source.png"
    let povrayArgs = ["+A", sourceFile]
        pro = proc povrayPath povrayArgs

    (ec, out, err) <- readCreateProcessWithExitCode pro ""

    res <- case ec of
      ExitSuccess   -> Right <$> BS.readFile resultFile
      ExitFailure e -> pure (Left err)
    pure (out, res)
