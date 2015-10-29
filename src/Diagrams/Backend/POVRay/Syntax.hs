{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
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

import           Diagrams.ThreeD.Types

import           Text.PrettyPrint.HughesPJ

import           Control.Lens

import           Data.Monoid hiding ((<>))

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

instance SDL s => SDL (Maybe s) where
  toSDL Nothing  = empty
  toSDL (Just s) = toSDL s

------------------------------------------------------------
-- Basics
------------------------------------------------------------

type Identifier = String

type Vector = V3 Double

instance SDL Vector where
  toSDL (V3 x y z) = text "<" <> hsep (punctuate comma (map toSDL [x,y,z])) <> text ">"

newtype VColor = RGB Vector deriving (Show, Eq)

instance SDL VColor where
  toSDL (RGB v) = text "rgb" <+> toSDL v

------------------------------------------------------------
-- Scene items
------------------------------------------------------------

-- | Top-level items that can occur in a scene.
data SceneItem = SICamera CameraType [CameraItem]
               | SIObject Object
               deriving Show

instance SDL SceneItem where
  toSDL (SICamera cType cItems) = block "camera"
                                  (toSDL cType:map toSDL cItems)
  toSDL (SIObject obj)    = toSDL obj

------------------------------------------------------------
-- Camera
------------------------------------------------------------

data CameraItem = CIVector CameraVector
                | CIModifier CameraModifier
                deriving Show

instance SDL CameraItem where
  toSDL (CIVector cv)   = toSDL cv
  toSDL (CIModifier cm) = toSDL cm

data CameraType = Perspective | Orthographic  -- TODO add more types?
                deriving Show


data CameraVector = CVLocation  Vector
                  | CVRight     Vector
                  | CVUp        Vector
                  | CVDirection Vector
                  | CVSky       Vector
                  deriving Show

instance SDL CameraType where
  toSDL Perspective = empty
  toSDL Orthographic = text "orthographic"

instance SDL CameraVector where
  toSDL (CVLocation v)  = text "location"  <+> toSDL v
  toSDL (CVRight v)     = text "right"     <+> toSDL v
  toSDL (CVUp v)        = text "up"        <+> toSDL v
  toSDL (CVDirection v) = text "direction" <+> toSDL v
  toSDL (CVSky v)       = text "sky"       <+> toSDL v

data CameraModifier = CMLookAt Vector
                    | CMAngle Double -- degrees
                    deriving Show

instance SDL CameraModifier where
  toSDL (CMLookAt v) = text "look_at" <+> toSDL v
  toSDL (CMAngle  d) = text "angle" <+> toSDL d

------------------------------------------------------------
-- Objects
------------------------------------------------------------

data Object = OFiniteSolid FiniteSolid
            | OLight LightSource
            deriving Show

instance SDL Object where
  toSDL (OFiniteSolid fs) = toSDL fs
  toSDL (OLight l)        = toSDL l

data ObjectModifier = OM {
  omTexture :: Texture,
  omTransf  :: Last TMatrix
  } deriving Show

instance Monoid ObjectModifier where
  mempty = OM mempty mempty
  mappend (OM a1 b1) (OM a2 b2) = OM (mappend a1 a2) (mappend b1 b2)

instance SDL ObjectModifier where
  toSDL om = vcat [
    toSDL $ omTexture om,
    lastToSDL "" $ omTransf om
                  ]

-- should be a list of 12 doubles
data TMatrix = TMatrix [Double]
             deriving Show

instance SDL TMatrix where
  toSDL (TMatrix ds) = text "matrix <"
                       <> (hcat . punctuate comma . map toSDL $ ds)
                       <> text ">"

-- May support more pigment & texture options in the future.
data Texture = Texture (Last VColor) TFinish
             deriving (Show, Eq)

instance Monoid Texture where
  mempty = Texture mempty mempty
  mappend (Texture c1 f1) (Texture c2 f2) =
    Texture (mappend c1 c2) (mappend f1 f2)

data TFinish = TFinish {
  tAmbient   :: Last Double,  -- POVRay allows a color here, instead of white light
  tDiffuse   :: Last Double,
  tSpecular  :: Last Double,
  tRoughness :: Last Double
  } deriving (Show, Eq)

instance Monoid TFinish where
  mempty = TFinish mempty mempty mempty mempty
  mappend (TFinish a1 d1 s1 r1) (TFinish a2 d2 s2 r2) =
    TFinish (mappend a1 a2) (mappend d1 d2) (mappend s1 s2) (mappend r1 r2)

lastToSDL :: SDL a => String -> Last a -> Doc
lastToSDL s x = case getLast x of
  Nothing -> mempty
  Just x' -> text s <+> toSDL x'

instance SDL Texture where
  toSDL t | t == mempty = mempty
  toSDL (Texture vc finish) = block "texture" $ case getLast vc of
    Nothing -> [toSDL finish]
    Just c -> [block "pigment" [toSDL c], toSDL finish]

instance SDL TFinish where
  toSDL finish
    | finish == mempty = mempty
    | otherwise = block "finish"  $
                  [ lastToSDL "ambient" $ tAmbient finish
                  , lastToSDL "diffuse" $ tDiffuse finish
                  , lastToSDL "specular" $ tSpecular finish
                  , lastToSDL "roughness" $ tRoughness finish]

------------------------------------------------------------
-- Finite solids
------------------------------------------------------------

data FiniteSolid = Sphere Vector Double ObjectModifier
                 | Box Vector Vector ObjectModifier
                 | Cone Vector Double Vector Double Bool ObjectModifier
                 | Union [FiniteSolid] ObjectModifier -- probably not what you want
                 | Merge [FiniteSolid] ObjectModifier -- real CSG union
                 | Intersection [FiniteSolid] ObjectModifier
                 | Difference [FiniteSolid] ObjectModifier
                 deriving Show

instance SDL FiniteSolid where
  toSDL (Sphere c r mods) = block "sphere" [cr,  toSDL mods]
    where cr = toSDL c <> comma <+> toSDL r
  toSDL (Box p1 p2 mods) = block "box" [corners, toSDL mods]
    where corners = toSDL p1 <> comma <+> toSDL p2
  toSDL (Cone p1 r1 p2 r2 o mods) = block "cone" [geom, open, toSDL mods] where
    open = if o then text " open" else empty
    geom = toSDL p1 <> comma <+> toSDL r1 <> comma <+>
           toSDL p2 <> comma <+> toSDL r2
  toSDL (Union solids mods) =
      block "union" $ map toSDL solids ++ [toSDL mods]
  toSDL (Merge solids mods) =
      block "merge" $ map toSDL solids ++ [toSDL mods]
  toSDL (Intersection solids mods) =
      block "intersection" $ map toSDL solids ++ [toSDL mods]
  toSDL (Difference solids mods) =
      block "difference" $ map toSDL solids ++ [toSDL mods]

------------------------------------------------------------
-- Light sources
------------------------------------------------------------

data LightSource = LightSource Vector VColor [LightModifier]
                 deriving Show

instance SDL LightSource where
  toSDL (LightSource loc c mods) = block "light_source" (lc : map toSDL mods)
    where lc = toSDL loc <> comma <+> toSDL c

data LightModifier = Parallel Vector
                   deriving Show

instance SDL LightModifier where
    toSDL (Parallel v) = text "parallel" $$ text "point_at" <+> toSDL v

makePrisms ''SceneItem
makePrisms ''Object
-- makePrisms ''ObjectModifier

getMods :: FiniteSolid -> ObjectModifier
getMods (Sphere _ _ ms)     = ms
getMods (Box _ _ ms)        = ms
getMods (Cone _ _ _ _ _ ms) = ms
getMods (Union _ ms) = ms
getMods (Merge _ ms) = ms
getMods (Intersection _ ms) = ms
getMods (Difference _ ms) = ms

setMods :: FiniteSolid -> ObjectModifier -> FiniteSolid
setMods (Sphere v r _) new         = Sphere v r new
setMods (Box p1 p2 _) new          = Box p1 p2 new
setMods (Cone p1 r1 p2 r2 o _) new = Cone p1 r1 p2 r2 o new
setMods (Union fs _) new = Union fs new
setMods (Merge fs _) new = Merge fs new
setMods (Intersection fs _) new = Intersection fs new
setMods (Difference fs _) new = Difference fs new

mods :: Lens' FiniteSolid ObjectModifier
mods = lens getMods setMods
