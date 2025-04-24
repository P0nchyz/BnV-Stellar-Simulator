module Engine (updateSystem) where

import Objects
import Vector

g :: Double
g = 6.4743e-11

gravitationalForce :: Body -> Body -> Vec2D
gravitationalForce body1 body2 =
  (-g) * mass body1 * mass body2 / distance ^ (2 :: Integer) ^* direction
  where
    r21 = position body2 - position body1
    distance = magnitude r21
    direction = normalize r21

netForce :: System -> Body -> Vec2D
netForce system body =
  let otherBodies = filter (/= body) system
  in foldr (\otherBody acc -> acc + gravitationalForce otherBody body) (Vec2D 0 0) otherBodies

applyForce :: Double -> Vec2D -> Body -> Body
applyForce timeStep force body =
  let acceleration = (1 / mass body) ^* force
      newVelocity = velocity body + (timeStep ^* acceleration)
      newPosition = position body + (timeStep ^* newVelocity)
  in body {position = newPosition, velocity = newVelocity}

updateSystem :: Double -> System -> System
updateSystem timeStep system =
  let updateBody body = applyForce timeStep force body where force = netForce system body
  in map updateBody system
