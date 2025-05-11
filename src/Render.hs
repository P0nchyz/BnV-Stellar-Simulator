module Render (renderSystem, update) where

import Graphics.Gloss
import Engine
import Objects
import Vector
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Units

renderSystem :: System -> Picture
renderSystem system = Pictures $ map renderBody system

scaleFunction :: Double -> Double
scaleFunction size
  | size < midSize = max minDisplay (minDisplay + (midDisplay - minDisplay) * (log10 size - logMin) / (logMid - logMin))
  | otherwise = midDisplay + (maxDisplay - midDisplay) * (log10 size - logMid) / (logMax - logMid)
  where
    maxDisplay = 200
    midDisplay = 100
    minDisplay = 1
    maxSize = 1 * sunRadius
    midSize = 4.97 * earthRadius
    minSize = 1000e3
    log10 = logBase 10
    logMax = log10 maxSize
    logMid = log10 midSize
    logMin = log10 minSize

renderBody :: Body -> Picture
renderBody body =
  let
    scaleFactor = 30e-9
    pos = scaleFactor ^* position body
    posX = realToFrac $ x pos
    posY = realToFrac $ y pos
    size = realToFrac $ scaleFunction $ radius body
    col = bodyColor body
  in
    case bodyType body of
      Star ->
        let Just (StarSpecific starData) = bodyData body
            glow = realToFrac $ min 3.0 $ sqrt (luminosity starData / 3.828e26)
            starColor = adjustColorByTemp col (temperature starData)
        in Pictures [
          Translate posX posY $ Color starColor $ ThickCircle (size * 0.1) size,
          Translate posX posY $ Color (withAlpha 0.2 starColor) $ ThickCircle (size * 0.1) (size * 1.1 * glow),
          Translate posX posY $ Color (withAlpha 0.1 starColor) $ ThickCircle (size * 0.1) (size * 1.2 * glow),
          Translate posX posY $ Color (withAlpha 0.05 starColor) $ ThickCircle (size * 0.1) (size * 1.5 * glow)
        ]
      Asteroid ->
        let
          posCola = (- (1e-3 ^* velocity body))
          posXCola = realToFrac $ x posCola
          posYCola = realToFrac $ y posCola
        in Pictures [
          Translate posX posY $ color col $ ThickCircle (size * 0.1) size,
          Translate posX posY $ color white $ line [(0,0), (posXCola, posYCola)]
        ]
      _ -> Translate posX posY $ Color col $ ThickCircle (size * 0.1) size

adjustColorByTemp :: Color -> Double -> Color
adjustColorByTemp baseColor temp
  | temp < 3500 = mixColors 0.5 0.5 baseColor red
  | temp > 10000 = mixColors 0.5 0.5 baseColor blue
  | otherwise = baseColor

update :: ViewPort -> Float -> System -> System
update _ dt = updateSystem $ 2419200 * realToFrac dt