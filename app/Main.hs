module Main (main) where

import Graphics.Gloss

import Objects
import Render
import Units
import Vector

window :: Display
window = FullScreen

background :: Color
background = black

mainStar :: Body
mainStar = Body {
  bodyType = Star,
  name = "Sun",
  mass = sunMass,
  radius = sunRadius,
  position = Vec2D 0 0,
  velocity = Vec2D 0 0,
  bodyColor = makeColor 1.0 1.0 0.0 1.0,
  bodyData = StarSpecific StarData {
    luminosity = solarLuminosity,
    temperature = 5772,
    metalicity = 0.0122,
    spectralClass = "G2V"
  }
}

planets :: [Body]
planets = [
  Body {
    bodyType = Planet,
    name = "Mercury",
    mass = 0.055 * earthMass,
    radius = 0.3829 * earthRadius,
    position = Vec2D (0.387098 * au) 0,
    velocity = Vec2D 0 47.36e3,
    bodyColor = makeColor 0.6 0.6 0.7 1.0,
    bodyData = PlanetSpecific PlanetData {
      planetType = "Terrestial",
      composition = ["Silicate", "Iron", "Nickel"]
    }
  },
  Body {
    bodyType = Planet,
    name = "Venus",
    mass = 0.815 * earthMass,
    radius = 0.9499 * earthRadius,
    position = Vec2D (0.7233 * au) 0,
    velocity = Vec2D 0 35.02e3,
    bodyColor = makeColor 0.4 0.8 0.8 1.0,
    bodyData = PlanetSpecific PlanetData {
      planetType = "Terrestial",
      composition = ["Silicate", "Iron", "Nickel"]
    }
  },
  Body {
    bodyType = Planet,
    name = "Earth",
    mass = earthMass,
    radius = earthRadius,
    position = Vec2D 149.6e9 0,
    velocity = Vec2D 0 29800,
    bodyColor = makeColor 0 0.4 0.8 1.0,
    bodyData = PlanetSpecific PlanetData {
      planetType = "Terrestial",
      composition = ["Silicate", "Iron", "Nickel"]
    }
  },
  Body {
    bodyType = Planet,
    name = "Mars",
    mass = 0.107 * earthMass,
    radius = 0.8 * earthRadius,
    position = Vec2D (1.523 * au) 0,
    velocity = Vec2D 0 24.07e3,
    bodyColor = makeColor 0.9 0.5 0.3 1.0,
    bodyData = PlanetSpecific PlanetData {
      planetType = "Terrestial",
      composition = ["Silicate", "Iron", "Nickel"]
    }
  },
  Body {
    bodyType = Planet,
    name = "Jupiter",
    mass = jupiterMass,
    radius = 10.973 * earthRadius,
    position = Vec2D (5.2038 * au) 0,
    velocity = Vec2D 0 13.06e3,
    bodyColor = makeColor 0.8 0.6 0.2 1.0,
    bodyData = PlanetSpecific PlanetData {
      planetType = "Gas Giant",
      composition = ["Hydrogen", "Helium", "Methane"]
    }
  },
  Body {
    bodyType = Planet,
    name = "Saturn",
    mass = 95.162 * earthMass,
    radius = 9.140 * earthRadius,
    position = Vec2D (9.5370 * au) 0,
    velocity = Vec2D 0 9.6724e3,
    bodyColor = makeColor 0.6 0.4 0.2 1.0,
    bodyData = PlanetSpecific PlanetData {
      planetType = "Gas Giant",
      composition = ["Hydrogen", "Helium", "Methane"]
    }
  },
  Body {
    bodyType = Planet,
    name = "Uranus",
    mass = 14.536 * earthMass,
    radius = 3.981 * earthRadius,
    position = Vec2D (19.1912 * au) 0,
    velocity = Vec2D 0 6.8352e3,
    bodyColor = makeColor 0.7 0.7 1.0 1.0,
    bodyData = PlanetSpecific PlanetData {
      planetType = "Gas Giant",
      composition = ["Hydrogen", "Helium", "Methane"]
    }
  },
  Body {
    bodyType = Planet,
    name = "Neptune",
    mass = 17.147 * earthMass,
    radius = 3.865 * earthRadius,
    position = Vec2D (30.0689 * au) 0,
    velocity = Vec2D 0 5.4778e3,
    bodyColor = makeColor 0.5 0.5 1.0 1.0,
    bodyData = PlanetSpecific PlanetData {
      planetType = "Gas Giant",
      composition = ["Hydrogen", "Helium", "Methane"]
    }
  }
  ]

stellarSystem :: System
stellarSystem = mainStar : planets

main :: IO ()
main = simulate window background 60 stellarSystem renderSystem update