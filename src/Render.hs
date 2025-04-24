module Render (renderSystem, update) where

import Graphics.Gloss
import Engine
import Objects
import Vector
import Graphics.Gloss.Data.ViewPort (ViewPort)

renderSystem :: System -> Picture
renderSystem system = Pictures $ map renderBody system

scaleFunction :: Double -> Float
scaleFunction size = max 1 $ realToFrac $ 7.8 * log (1e-6 * size)

renderBody :: Body -> Picture
renderBody body =
  let
    scaleFactor = 1e-9
    pos = scaleFactor ^* position body
    posX = realToFrac $ x pos
    posY = realToFrac $ y pos
    size = scaleFunction $ radius body
    col = bodyColor body
  in
    case bodyType body of
      Star ->
        let StarSpecific starData = bodyData body
            glow = realToFrac $ min 3.0 $ sqrt (luminosity starData / 3.828e26)
            starColor = adjustColorByTemp col (temperature starData)
        in Pictures [
          Translate posX posY $ Color starColor $ ThickCircle (size * 0.1) size,
          Translate posX posY $ Color (withAlpha 0.2 starColor) $ ThickCircle (size * 0.1) (size * 1.1 * glow),
          Translate posX posY $ Color (withAlpha 0.1 starColor) $ ThickCircle (size * 0.1) (size * 1.2 * glow),
          Translate posX posY $ Color (withAlpha 0.05 starColor) $ ThickCircle (size * 0.1) (size * 1.5 * glow)
        ]
      _ -> Translate posX posY $ Color col $ ThickCircle (size * 0.1) size 

adjustColorByTemp :: Color -> Double -> Color
adjustColorByTemp baseColor temp
  | temp < 3500 = mixColors 0.5 0.5 baseColor red
  | temp > 10000 = mixColors 0.5 0.5 baseColor blue
  | otherwise = baseColor

update :: ViewPort -> Float -> System -> System
update _ dt = updateSystem $ 2419200 * realToFrac dt