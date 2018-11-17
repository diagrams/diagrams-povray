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
  ( POVRayOptions (..)
  , runPovray
  ) where

import System.Process
import System.Exit

data POVRayOptions

povrayPath :: FilePath
povrayPath = "povray"

runPovray :: POVRayOptions -> FilePath -> IO ()
runPovray opts path = do
  let povrayArgs = ["+A", path]
      pro = (proc povrayPath povrayArgs)
        { std_in  = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

  (ec, out, err) <- readCreateProcessWithExitCode pro ""

  case ec of
    ExitSuccess   -> pure ()
    ExitFailure e -> do
      putStrLn $ "povray failure: " <> show e
      putStrLn err
