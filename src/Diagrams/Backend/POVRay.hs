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

import Control.Lens ((^.), (<>~), view)
import Data.Maybe

import Diagrams.Core.Transform

import Diagrams.Prelude hiding (fromDirection, view)
import Diagrams.ThreeD

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

  withStyle _ s _ (Pov is) = Pov $ map (setTexture s) is

  doRender _ _ (Pov items) = PP.render . PP.vcat . map toSDL $ items

instance Renderable Ellipsoid POVRay where
  render _ (Ellipsoid t) = Pov [SIObject . OFiniteSolid $ s]
    where s = Sphere zeroV 1 [povrayTransf t]

-- For perspective projection, forLen tells POVRay the horizontal
-- field of view, and CVRight specifies the aspect ratio of the view.
-- For orthographic projection, rightLen & upLen are the actual window
-- dimensions, and forLen is ignored by POVRay.
instance Renderable (Camera PerspectiveLens) POVRay where
  render _ c = Pov [ SICamera cType [
    CIVector . CVLocation . vector $ l
    , CIVector . CVDirection . vector . unr3 $ forLen *^ forUnit
    , CIVector . CVUp . vector . unr3 $ upUnit
    , CIVector . CVRight . vector . unr3 $ rightLen *^ rightUnit
    ]]
    where
      l = unp3 . camLoc $ c
      (PerspectiveLens h v) = camLens c
      forUnit = fromDirection . asSpherical . camForward $ c
      forLen = 0.5*rightLen/tan(h^.rad/2)
      upUnit =  fromDirection . asSpherical . camUp $ c
      rightUnit = fromDirection . asSpherical . camRight $ c
      rightLen = angleRatio h v
      cType = Perspective

instance Renderable (Camera OrthoLens) POVRay where
  render _ c = Pov [ SICamera Orthographic [
    CIVector . CVLocation . vector $ l
    , CIVector . CVDirection . vector . unr3 $ forUnit
    , CIVector . CVUp . vector . unr3 $ v *^ upUnit
    , CIVector . CVRight . vector . unr3 $ h *^ rightUnit
    ]]
    where
      l = unp3 . camLoc $ c
      (OrthoLens h v) = camLens c
      forUnit = fromDirection . asSpherical . camForward $ c
      upUnit =  fromDirection . asSpherical . camUp $ c
      rightUnit = fromDirection . asSpherical . camRight $ c

instance Renderable ParallelLight POVRay where
    render _ (ParallelLight v c) = Pov [SIObject . OLight $ LightSource pos c' [
        Parallel v' ]] where
      pos = vector . unp3 $ origin .-^ (1000 *^ v)
      v' =  vector . unp3 $ origin
      c' = convertColor c

instance Renderable PointLight POVRay where
    render _ (PointLight p c) =
        Pov [SIObject . OLight $ LightSource pos c' []] where
          pos = vector $ unp3 p
          c' = convertColor c

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

convertColor :: Color c => c -> VColor
convertColor c = RGB $ vector (r, g, b) where
  (r, g, b, _) = colorToSRGBA c

setTexture :: Style R3 -> SceneItem -> SceneItem
setTexture sty = _SIObject . _OFiniteSolid . mods <>~
                 [OMTexture (mkFinish sty:mkPigment sty)]

mkPigment :: Style R3 -> [Texture]
mkPigment sty = Pigment . convertColor . view surfaceColor <$> catMaybes [getAttr sty]

mkFinish :: Style R3 -> Texture
mkFinish sty = Finish . catMaybes $ [
                     TAmbient . view _Ambient <$> getAttr sty,
                     TDiffuse . view _Diffuse <$> getAttr sty,
                     TSpecular . view (_Highlight . specularIntensity) <$> getAttr sty,
                     TRoughness . view (_Highlight . specularSize) <$> getAttr sty
                     ]
