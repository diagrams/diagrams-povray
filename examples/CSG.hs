-- | A test of (some of) the CSG functionality

module CSG where

import           Diagrams.Prelude

import           Diagrams.Backend.POVRay

main = putStrLn $ renderDia POVRay POVRayOptions ex


ex :: Diagram POVRay
ex = shape <> cameraLight

shape :: Diagram POVRay
shape = sc blue . diffuse 0.6 . ambient 0.2 . highlight (Specular 0.2 10) .
        scale 20 . skin $ difference (cube # translateZ (-1)) sphere

cameraLight :: Diagram POVRay
cameraLight = facing_ZCamera mm50 # translate (V3 0.5 0.5 100) <> parallelLight (direction $ V3 1 (-1) (-2)) white
