{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.POVRay.Syntax
-- Copyright   :  (c) 2011 Diagrams-povray team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A (very partial) AST for POV-Ray syntax, useful for building up an
-- abstract POVRay scene and then serializing it to a POVRay scene
-- description file.  Parser not included.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.POVRay.Syntax where

import Text.PrettyPrint.HughesPJ

------------------------------------------------------------
-- Pretty-printing
------------------------------------------------------------

-- | Type class for things that can be pretty-printed as POVRay source
--   code.
class SDL p where
  toSDL :: p -> Doc

-- | Helper function to generate a labeled block like
--
--   > label {
--   >   item1
--   >   item2
--   >   ...
--   > }
--
block :: String -> [Doc] -> Doc
block label items = text label <+> lbrace $$ nest 4 (vcat items) $$ rbrace

instance SDL String where
  toSDL = text

instance SDL Double where
  toSDL = double

instance SDL () where
  toSDL _ = empty

------------------------------------------------------------
-- Basics
------------------------------------------------------------

type Identifier = String

data Vector = VecLit Double Double Double

instance SDL Vector where
  toSDL (VecLit x y z) = text "<" <> hsep (punctuate comma (map toSDL [x,y,z])) <> text ">"

data Color = RGB Vector

instance SDL Color where
  toSDL (RGB v) = text "rgb" <+> toSDL v

------------------------------------------------------------
-- Scene items
------------------------------------------------------------

-- | Top-level items that can occur in a scene.
data SceneItem = SICamera [CameraItem]
               | SIObject Object

instance SDL SceneItem where
  toSDL (SICamera cItems) = block "camera" (map toSDL cItems)
  toSDL (SIObject obj)    = toSDL obj

------------------------------------------------------------
-- Camera
------------------------------------------------------------

data CameraItem = CIVector CameraVector
                | CIModifier CameraModifier

instance SDL CameraItem where
  toSDL (CIVector cv)   = toSDL cv
  toSDL (CIModifier cm) = toSDL cm


data CameraVector = CVLocation  Vector
                  | CVRight     Vector
                  | CVUp        Vector
                  | CVDirection Vector
                  | CVSky       Vector

instance SDL CameraVector where
  toSDL (CVLocation v)  = text "location"  <+> toSDL v
  toSDL (CVRight v)     = text "right"     <+> toSDL v
  toSDL (CVUp v)        = text "up"        <+> toSDL v
  toSDL (CVDirection v) = text "direction" <+> toSDL v
  toSDL (CVSky v)       = text "sky"       <+> toSDL v

data CameraModifier = CMLookAt Vector

instance SDL CameraModifier where
  toSDL (CMLookAt v) = text "look_at" <+> toSDL v

------------------------------------------------------------
-- Objects
------------------------------------------------------------

data Object = OFiniteSolid FiniteSolid
            | OLight LightSource

instance SDL Object where
  toSDL (OFiniteSolid fs) = toSDL fs
  toSDL (OLight l)        = toSDL l

data ObjectModifier = OMPigment Pigment

instance SDL ObjectModifier where
  toSDL (OMPigment p) = toSDL p

data Pigment = PColor Color

instance SDL Pigment where
  toSDL (PColor c) = block "pigment" [toSDL c]

------------------------------------------------------------
-- Finite solids
------------------------------------------------------------

data FiniteSolid = Sphere Vector Double [ObjectModifier]

instance SDL FiniteSolid where
  toSDL (Sphere c r mods) = block "sphere" (cr : map toSDL mods)
    where cr = toSDL c <> comma <+> toSDL r

------------------------------------------------------------
-- Light sources
------------------------------------------------------------

data LightSource = LightSource Vector Color [LightModifier]

instance SDL LightSource where
  toSDL (LightSource loc c mods) = block "light_source" (lc : map toSDL mods)
    where lc = toSDL loc <> comma <+> toSDL c

type LightModifier = ()
