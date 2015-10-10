{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

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
  ( POVRay (..)  -- backend token
  , Options (..) -- rendering options
  ) where

import           Control.Lens                   (view)
import           Data.Maybe
import           Data.Monoid                    (Last (..))
import           Data.Tree
import           Data.Typeable
import qualified Text.PrettyPrint.HughesPJ      as PP

import           Diagrams.Core.Transform
import           Diagrams.Core.Types
import           Diagrams.Prelude               as D hiding (Last (..), view)

import           Diagrams.Backend.POVRay.Syntax as P

data POVRay = POVRay
  deriving (Eq,Ord,Read,Show,Typeable)

type instance V POVRay = V3
type instance N POVRay = Double

instance Monoid (Render POVRay V3 Double) where
  mempty  = Pov []
  (Pov i1) `mappend` (Pov i2) = Pov (i1 ++ i2)

instance Backend POVRay V3 Double where
  data Render  POVRay V3 Double = Pov [SceneItem]
  type Result  POVRay V3 Double = String
  data Options POVRay V3 Double = POVRayOptions

  renderRTree _ _ rt  = PP.render . PP.vcat . map toSDL . go $ rt where
    go :: RTree POVRay V3 Double a -> [SceneItem]
    go (Node (RPrim p) _)   = unPov $ render POVRay p
    go (Node (RStyle s) ts) = map (setTexture s) . concatMap go $ ts
    go (Node _ ts)          = concatMap go $ ts

unPov :: Render POVRay V3 Double -> [SceneItem]
unPov (Pov is) = is

wrapSolid :: P.FiniteSolid -> Render POVRay V3 Double
wrapSolid = Pov . (:[]) . SIObject . OFiniteSolid

class ToSolid t where
    toSolid :: t -> P.FiniteSolid

instance ToSolid (Ellipsoid Double) where
    toSolid (Ellipsoid t) = Sphere zero 1 (povrayTransf t)

instance Renderable  (Ellipsoid Double) POVRay where
    render _ = wrapSolid . toSolid

instance ToSolid (D.Box Double) where
    toSolid (D.Box t) = P.Box zero (V3 1 1 1) (povrayTransf t)

instance Renderable (D.Box Double) POVRay where
    render _ = wrapSolid . toSolid

instance ToSolid (Frustum Double) where
    toSolid (Frustum r0 r1 t) = Cone zero r0 (V3 0 0 1) r1 False (povrayTransf t)

instance Renderable (Frustum Double) POVRay where
    render _ = wrapSolid . toSolid

instance ToSolid (CSG Double) where
    toSolid (CsgEllipsoid prim) = toSolid prim
    toSolid (CsgBox prim) = toSolid prim
    toSolid (CsgFrustum prim) = toSolid prim
    toSolid (CsgUnion ps) = Union (map toSolid ps) mempty
    toSolid (CsgIntersection ps) = Intersection (map toSolid ps) mempty
    toSolid (CsgDifference pr1 pr2) = Difference [toSolid pr1, toSolid pr2] mempty

instance Renderable (CSG Double) POVRay where
  render _ = wrapSolid . toSolid

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
      forUnit   = fromDirection . camForward $ c
      forLen    = 0.5*rightLen/tan(h^.rad/2)
      upUnit    = fromDirection . camUp $ c
      rightUnit = fromDirection . camRight $ c
      rightLen  = angleRatio h v
      cType     = Perspective

instance Renderable (Camera OrthoLens Double) POVRay where
  render _ c = Pov [ SICamera Orthographic
    [ CIVector . CVLocation  $ l
    , CIVector . CVDirection $ forUnit
    , CIVector . CVUp        $ v *^ upUnit
    , CIVector . CVRight     $ h *^ rightUnit
    ]]
    where
      l = camLoc c .-. origin
      (OrthoLens h v) = camLens c
      forUnit         = fromDirection . camForward $ c
      upUnit          = fromDirection . camUp $ c
      rightUnit       = fromDirection . camRight $ c

instance Renderable (ParallelLight Double) POVRay where
  render _ (ParallelLight v c)
    = Pov [SIObject . OLight $ LightSource pos c' [ Parallel zero ]]
      where
        pos = negated (1000 *^ v)
        c' = convertColor c

instance Renderable (PointLight Double) POVRay where
  render _ (PointLight (P pos) (convertColor -> c))
    = Pov [SIObject . OLight $ LightSource pos c []]

povrayTransf :: T3 Double -> ObjectModifier
povrayTransf t = OM mempty . Last . Just . TMatrix . concat . matrixHomRep $ t

convertColor :: Color c => c -> VColor
convertColor (colorToSRGBA -> (r,g,b,_)) = P.RGB $ V3 r g b

setTexture :: Style V3 Double -> SceneItem -> SceneItem
setTexture sty = _SIObject . _OFiniteSolid . mods <>~
                 (OM (Texture (mkPigment sty) (mkFinish sty)) mempty)

mkPigment :: Style V3 Double -> Last VColor
mkPigment = Last . fmap convertColor . view _sc

  -- toListOf (_sc . _Just . to (Pigment . convertColor))

mkFinish :: Style V3 Double -> TFinish
mkFinish sty = TFinish
               (Last $ sty ^. _ambient)
               (Last $ sty ^. _diffuse)
               (Last $ hl  ^? _Just . specularIntensity)
               (Last $ hl  ^? _Just . specularSize)
  where hl = sty ^. _highlight
