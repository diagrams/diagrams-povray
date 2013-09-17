{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , DeriveDataTypeable
           , ViewPatterns
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.POVRay
-- Copyright   :  (c) 2011 Diagrams-povray team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- An experimental backend for three-dimensional diagrams.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.POVRay

  ( POVRay(..)       -- backend token
  ,  Options(..)  -- rendering options
  ) where

import Diagrams.Core.Transform

import Diagrams.Prelude
import Diagrams.ThreeD.Types
import Diagrams.ThreeD.Shapes

import Diagrams.Backend.POVRay.Syntax

import Data.Typeable

import qualified Text.PrettyPrint.HughesPJ as PP

data POVRay = POVRay
  deriving (Eq,Ord,Read,Show,Typeable)

instance Monoid (Render POVRay R3) where
  mempty  = Pov []
  (Pov i1) `mappend` (Pov i2) = Pov (i1 ++ i2)

instance Backend POVRay R3 where
  data Render  POVRay R3 = Pov [SceneItem]
  type Result  POVRay R3 = String
  data Options POVRay R3 = POVRayOptions

  withStyle _ _ _ p = p

  doRender _ _ (Pov items) = PP.render . PP.vcat . map toSDL $ items

instance Renderable Ellipsoid POVRay where
  render _ (Ellipsoid t) = Pov [SIObject . OFiniteSolid $ s]
    where s = Sphere zeroV 1 [povrayTransf t]

povrayTransf :: T3 -> ObjectModifier
povrayTransf t = OMTransf $
                 TMatrix [ v00, v01, v02
                         , v10, v11, v12
                         , v20, v21, v22
                         , v30, v31, v32 ]
  where (unr3 -> (v00, v01, v02)) = apply t (r3 (1,0,0))
        (unr3 -> (v10, v11, v12)) = apply t (r3 (0,1,0))
        (unr3 -> (v20, v21, v22)) = apply t (r3 (0,0,1))
        (unr3 -> (v30, v31, v32)) = transl t

vector :: (Double, Double, Double) -> Vector
vector (x, y, z) = VecLit x y z
