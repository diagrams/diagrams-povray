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

import           Control.Lens ((^.), (<>~), view)
import           Data.Maybe
import           Data.Tree
import           Data.Typeable
import qualified Text.PrettyPrint.HughesPJ as PP

import           Diagrams.Core.Transform
import           Diagrams.Core.Types
import           Diagrams.Prelude as D hiding (view)

import           Diagrams.Backend.POVRay.Syntax as P

data POVRay = POVRay
  deriving (Eq,Ord,Read,Show,Typeable)

instance Monoid (Render POVRay V3 Double) where
  mempty  = Pov []
  (Pov i1) `mappend` (Pov i2) = Pov (i1 ++ i2)

instance Backend POVRay V3 Double where
  data Render  POVRay V3 Double = Pov [SceneItem]
  type Result  POVRay V3 Double = String
  data Options POVRay V3 Double = POVRayOptions

  renderRTree _ _ rt  = PP.render . PP.vcat . map toSDL . unPov . go $ rt where
    -- pmap :: (SceneItem -> SceneItem) -> Render POVRay V3 Double -> Render POVRay V3 Double
    -- pmap f (Pov is) = POV $ map f is
    unPov (Pov is) = is
    go :: RTree POVRay V3 Double a -> Render POVRay V3 Double
    go (Node (RPrim p) _) = render POVRay p
    go (Node (RStyle s) ts) = Pov . map (setTexture s) . concat . map (unPov . go) $ ts
    go (Node _ ts) = Pov . concat . map (unPov . go) $ ts

instance Renderable (Ellipsoid Double) POVRay where
  render _ (Ellipsoid t) = Pov [SIObject . OFiniteSolid $ s]
    where s = Sphere zero 1 [povrayTransf t]

instance Renderable (D.Box Double) POVRay where
    render _ (D.Box t) = Pov [SIObject . OFiniteSolid $ box]
      where box = P.Box zero (V3 1 1 1) [povrayTransf t]

instance Renderable (Frustum Double) POVRay where
    render _ (Frustum r0 r1 t) = Pov [SIObject . OFiniteSolid $ f]
      where f = Cone zero r0 (V3 0 0 1) r1 False [povrayTransf t]

-- For perspective projection, forLen tells POVRay the horizontal
-- field of view, and CVRight specifies the aspect ratio of the view.
-- For orthographic projection, rightLen & upLen are the actual window
-- dimensions, and forLen is ignored by POVRay.
instance Renderable (Camera PerspectiveLens Double) POVRay where
  render _ c = Pov [ SICamera cType [
    CIVector . CVLocation $ l
    , CIVector . CVDirection $ forLen *^ forUnit
    , CIVector . CVUp  $ upUnit
    , CIVector . CVRight $ rightLen *^ rightUnit
    ]]
    where
      l = camLoc c .-. origin
      (PerspectiveLens h v) = camLens c
      forUnit = fromDirection . camForward $ c
      forLen = 0.5*rightLen/tan(h^.rad/2)
      upUnit =  fromDirection . camUp $ c
      rightUnit = fromDirection . camRight $ c
      rightLen = angleRatio h v
      cType = Perspective

instance Renderable (Camera OrthoLens Double) POVRay where
  render _ c = Pov [ SICamera Orthographic [
    CIVector . CVLocation  $ l
    , CIVector . CVDirection $ forUnit
    , CIVector . CVUp  $ v *^ upUnit
    , CIVector . CVRight $ h *^ rightUnit
    ]]
    where
      l = camLoc c .-. origin
      (OrthoLens h v) = camLens c
      forUnit = fromDirection . camForward $ c
      upUnit =  fromDirection . camUp $ c
      rightUnit = fromDirection . camRight $ c

instance Renderable (ParallelLight Double) POVRay where
    render _ (ParallelLight v c) = Pov [SIObject . OLight $ LightSource pos c' [
        Parallel zero ]] where
      pos = negated (1000 *^ v)
      c' = convertColor c

instance Renderable (PointLight Double) POVRay where
    render _ (PointLight p c) =
        Pov [SIObject . OLight $ LightSource pos c' []] where
          pos = p .-. origin
          c' = convertColor c

povrayTransf :: Transformation V3 Double -> ObjectModifier
povrayTransf t = OMTransf $
                 TMatrix [ v00, v01, v02
                         , v10, v11, v12
                         , v20, v21, v22
                         , v30, v31, v32 ]
  where (unr3 -> (v00, v01, v02)) = apply t (r3 (1,0,0))
        (unr3 -> (v10, v11, v12)) = apply t (r3 (0,1,0))
        (unr3 -> (v20, v21, v22)) = apply t (r3 (0,0,1))
        (unr3 -> (v30, v31, v32)) = transl t

convertColor :: Color c => c -> VColor
convertColor c = RGB $ r3 (r, g, b) where
  (r, g, b, _) = colorToSRGBA c

setTexture :: Style V3 Double -> SceneItem -> SceneItem
setTexture sty = _SIObject . _OFiniteSolid . mods <>~
                 [OMTexture (mkFinish sty:mkPigment sty)]

mkPigment :: Style V3 Double -> [P.Texture]
mkPigment sty = Pigment . convertColor . view surfaceColor <$> catMaybes [getAttr sty]

mkFinish :: Style V3 Double -> P.Texture
mkFinish sty = Finish . catMaybes $ [
                     TAmbient . view _Ambient <$> getAttr sty,
                     TDiffuse . view _Diffuse <$> getAttr sty,
                     TSpecular . view (_Highlight . specularIntensity) <$> getAttr sty,
                     TRoughness . view (_Highlight . specularSize) <$> getAttr sty
                     ]
