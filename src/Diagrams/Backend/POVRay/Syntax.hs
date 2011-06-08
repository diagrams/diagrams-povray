{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.POVRay.Syntax
-- Copyright   :  (c) 2011 Diagrams-povray team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- (Partial) AST for POV-Ray syntax.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.POVRay.Syntax

  (

  ) where

import Text.PrettyPrint.HughesPJ

class Pretty p where
  ppr :: p -> Doc

block :: String -> [Doc] -> Doc
block label items = text label <+> lbrace $$ nest 4 (vcat items) $$ rbrace

data SceneItem = {- SIDirective Directive -}
                 SICamera [CameraItem]
               | SIObject Object
               {- | SIAtmosphere Atmosphere -}
               {- | SIGlobalSettings [GlobalItem] -}

instance Pretty SceneItem where
  ppr (SICamera cItems) = block "camera" (map ppr cItems)
  ppr (SIObject obj)    = ppr obj

data CameraItem = {- CIType CameraType -}
                  CIVector CameraVector
                | CIModifier CameraModifier
                {- | CIId Identifier -}

instance Pretty CameraItem where
  ppr (CIVector cv)   = ppr cv
  ppr (CIModifier cm) = ppr cm

data CameraVector = CVLocation  Vector
                  | CVRight     Vector
                  | CVUp        Vector
                  | CVDirection Vector
                  | CVSky       Vector

instance Pretty CameraVector where
  ppr (CVLocation v)  = text "location"  <+> ppr v
  ppr (CVRight v)     = text "right"     <+> ppr v
  ppr (CVUp v)        = text "up"        <+> ppr v
  ppr (CVDirection v) = text "direction" <+> ppr v
  ppr (CVSky v)       = text "sky"       <+> ppr v

data CameraModifier = {- CMAngle Double ?? -}
                      CMLookAt Vector
                    {- | CMBlurSamples Int
                       | CMAperture Double
                       | CMFocalPoint Vector
                       | CMConfidence Double
                       | CMVariance Double
                       | CMNormal Normal
                       | CMTransform Transformation
                    -}

instance Pretty CameraModifier where
  ppr (CMLookAt v) = text "look_at" <+> ppr v

type Identifier = String

instance Pretty String where
  ppr = text

data Vector = VecLit Double Double Double

instance Pretty Vector where
  ppr (VecLit x y z) = text "<" <> hsep (punctuate comma (map ppr [x,y,z])) <> text ">"

instance Pretty Double where
  ppr = double

data Object = OFiniteSolid FiniteSolid
  --          | OFinitePatch FinitePatch
  --          | OInfiniteSolid InfiniteSolid
  --          | OIsosurface Isosurface
  --          | OParametric Parametric
  --          | OCSG CSG
            | OLight LightSource
  --          | Object Identifier [ObjectModifier]

instance Pretty Object where
  ppr (OFiniteSolid fs) = ppr fs
  ppr (OLight l)        = ppr l

data FiniteSolid = Sphere Vector Double [ObjectModifier]
  --  XXX other stuff

instance Pretty FiniteSolid where
  ppr (Sphere c r mods) = block "sphere" (cr : map ppr mods)
    where cr = ppr c <> comma <+> ppr r

data ObjectModifier = OMPigment Pigment
  --                  | OMTransform Transformation
  -- XXX lots of other stuff!

instance Pretty ObjectModifier where
  ppr (OMPigment p) = ppr p

data Pigment = PColor Color
  -- XXX other stuff

instance Pretty Pigment where
  ppr (PColor c) = block "pigment" [ppr c]

data LightSource = LightSource Vector Color [LightModifier]

instance Pretty LightSource where
  ppr (LightSource loc c mods) = block "light_source" (lc : map ppr mods)
    where lc = ppr loc <> comma <+> ppr c

data Color = RGB Vector
  --         | RBGF Vector4
  --         | RGBT Vector4
  --         | RGBFT Vector5

instance Pretty Color where
  ppr (RGB v) = text "rgb" <+> ppr v

type LightModifier = ()  -- XXX

instance Pretty () where
  ppr _ = empty