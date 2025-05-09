module Objects (
  BodyType(..),
  Body(..),
  System,
  BodyData(..),
  StarData(..),
  PlanetData(..),
  MoonData(..),
  fromBody
) where

import Vector
import Graphics.Gloss (Color)

data BodyType = Star | Planet | Moon | Asteroid deriving (Show, Eq)

data Body = Body {
  bodyType :: BodyType,
  name :: String,
  mass :: Double,
  radius :: Double,
  position :: Vec2D,
  velocity :: Vec2D,
  bodyColor :: Color,
  bodyData :: BodyData
} deriving (Show, Eq)

type System = [Body]

data BodyData =
    StarSpecific StarData
  | PlanetSpecific PlanetData
  | MoonSpecific MoonData
  deriving (Show, Eq)

data StarData = StarData {
  luminosity :: Double,
  temperature :: Double,
  metalicity :: Double,
  spectralClass :: String
} deriving (Show, Eq)

data PlanetData = PlanetData {
  planetType :: String,
  composition :: [String]
} deriving (Show, Eq)

data MoonData = MoonData {
  orbits :: Body
} deriving (Show, Eq)

class BodyAction a where
  fromBody :: (Body -> a) -> Body -> a -> a

instance BodyAction Double where
  fromBody action body f = f * action body

instance BodyAction Vec2D where
  fromBody action body v = v + action body