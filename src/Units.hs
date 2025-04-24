module Units where

-- Distances
au :: Double  -- Astronomical Unit in meters
au = 1.495978707e11

lightYear :: Double  -- Light year in meters
lightYear = 9.4607e15

-- Time
day :: Double  -- Day in seconds
day = 86400

year :: Double  -- Julian year in seconds
year = 365.25 * day

-- Mass
earthMass :: Double  -- Earth's mass in kilograms
earthMass = 5.9722e24

sunMass :: Double  -- Sun's mass in kilograms
sunMass = 1.9885e30

jupiterMass :: Double
jupiterMass = 1.898e27

-- Radius
earthRadius :: Double  -- Earth's radius in meters
earthRadius = 6.378e6

sunRadius :: Double  -- Sun's radius in meters
sunRadius = 6.9634e8

-- Luminosity
solarLuminosity :: Double  -- Sun's luminosity in watts
solarLuminosity = 3.828e26