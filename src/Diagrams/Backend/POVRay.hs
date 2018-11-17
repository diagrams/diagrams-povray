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
import           Data.Monoid                    (Last (..))
import           Data.Tree
import           Data.Typeable
import qualified Text.PrettyPrint.HughesPJ      as PP

import           Diagrams.Prelude               as D hiding (Last (..), view)
import           Geometry
import           Geometry.ThreeD.Shapes         as G

import           Diagrams.Backend.Compile
import           Diagrams.Backend.POVRay.Syntax as P
import           Diagrams.ThreeD.Attributes
import           Diagrams.Types

data POVRay = POVRay
  deriving (Eq,Ord,Read,Show,Typeable)

type instance V POVRay = V3
type instance N POVRay = Double

instance Backend POVRay where
  type Result POVRay = [SceneItem]
  data Options POVRay = POVRayOptions

  backendInfo _ = povrayInfo
  renderDiaT opts dia = (undefined, mempty, r) where
    -- (sz, t, dia') = adjustSize (opts^.sizeSpec) dia
    -- b = undefined
    r = toRender mempty dia

toRender :: T3 Double -> Diagram V3 -> [SceneItem]
toRender = foldDia renderPrim renderAnnot
  where
    renderPrim t3 attrs prim = case renderPrimitive t3 attrs prim of
      Just r  -> r
      Nothing -> error $ "Unknown primitive"

renderPrimitive
  :: T3 Double -> Attributes -> Prim V3 Double -> Maybe [SceneItem]
renderPrimitive t3 attrs = (fmap . fmap . map) (setTexture attrs) $ \case
  Cube_          -> Just $ wrapSolid $ toSolid t3 G.Cube
  Frustum_ r0 r1 -> Just $ wrapSolid $ Cone zero r0 (V3 0 0 1) r1 False (povrayTransf t3)
  CSG_ csg -> Just $ wrapSolid $ toSolid t3 csg
  PointLight_ (P pos) (convertColor -> c) -> Just [SIObject . OLight $ LightSource pos c []]
  ParallelLight_ v (convertColor -> c) -> Just [SIObject . OLight $ LightSource pos c [ Parallel zero ]]
    where pos = negated (1000 *^ v)
  Prim_ c -> Just $ renderOrthoCam c
  Prim_ c -> Just $ renderPerspectiveCam c
  Prim _         -> Nothing

renderAnnot :: Annotation V3 Double -> [SceneItem] -> [SceneItem]
renderAnnot a = id

wrapSolid :: P.FiniteSolid -> [SceneItem]
wrapSolid = (:[]) . SIObject . OFiniteSolid

class ToSolid t where
  toSolid :: T3 Double -> t -> P.FiniteSolid

instance n ~ Double => ToSolid (Sphere n) where
  toSolid t G.Sphere = P.Sphere zero 1 (povrayTransf t)

instance n ~ Double => ToSolid (G.Cube n) where
  toSolid t G.Cube = P.Box zero (V3 1 1 1) (povrayTransf t)

instance n ~ Double => ToSolid (Frustum n) where
    toSolid t (Frustum r0 r1) = Cone zero r0 (V3 0 0 1) r1 False (povrayTransf t)

instance n ~ Double => ToSolid (CSG n) where
  toSolid t0 (CsgEllipsoid t) = toSolid (t0 <> t) G.Sphere
  toSolid t0 (CsgBox t) = toSolid (t0 <> t) G.Cube
  toSolid t0 (CsgFrustum r0 r1 t) = toSolid (t0 <> t) (Frustum r0 r1)
  toSolid t0 (CsgUnion ps) = Union (map (toSolid t0) ps) mempty
  toSolid t0 (CsgIntersection ps) = Intersection (map (toSolid t0) ps) mempty
  toSolid t0 (CsgDifference pr1 pr2) = Difference [toSolid t0 pr1, toSolid t0 pr2] mempty

-- For perspective projection, forLen tells POVRay the horizontal
-- field of view, and CVRight specifies the aspect ratio of the view.
-- For orthographic projection, rightLen & upLen are the actual window
-- dimensions, and forLen is ignored by POVRay.
renderPerspectiveCam :: Camera PerspectiveLens Double -> [SceneItem]
renderPerspectiveCam c =
  [ SICamera Perspective
    [ CIVector . CVLocation $ l
    , CIVector . CVDirection $ forLen *^ forUnit
    , CIVector . CVUp  $ upUnit
    , CIVector . CVRight $ rightLen *^ rightUnit
    ]
  ]
    where
      l = view cameraLoc c .-. origin
      (PerspectiveLens h v _ _) = view cameraLens c
      (forUnit,rightUnit) = camForwardRight c
      forLen    = 0.5*rightLen/tan(h^.rad/2)
      upUnit    = view camUp  c
      rightLen  = angleRatio h v

renderOrthoCam :: Camera OrthoLens Double -> [SceneItem]
renderOrthoCam c =
  [ SICamera Orthographic
    [ CIVector . CVLocation  $ l
    , CIVector . CVDirection $ forUnit
    , CIVector . CVUp        $ v *^ upUnit
    , CIVector . CVRight     $ h *^ rightUnit
    ]
  ]
    where
      l = view cameraLoc c .-. origin
      (OrthoLens h v _)   = view cameraLens c
      upUnit              = view camUp c
      (forUnit,rightUnit) = camForwardRight c

povrayTransf :: T3 Double -> ObjectModifier
povrayTransf t = OM mempty . Last . Just . TMatrix . concat . matrixHomRep $ t

convertColor :: Color c => c -> VColor
convertColor (colorToSRGBA -> (r,g,b,_)) = P.RGB $ V3 r g b

setTexture :: Attributes -> SceneItem -> SceneItem
setTexture sty = _SIObject . _OFiniteSolid . mods <>~
                 (OM (Texture (mkPigment sty) (mkFinish sty)) mempty)

mkPigment :: Attributes -> Last VColor
mkPigment = Last . fmap convertColor . getAttr _SurfaceColor

mkFinish :: Attributes -> TFinish
mkFinish attr = TFinish
               (Last $ getAttr _Ambient attr)
               (Last $ getAttr _Diffuse attr)
               (Last $ getAttr (_Highlight . specularIntensity) attr)
               (Last $ getAttr (_Highlight . specularSize) attr)
