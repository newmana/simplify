{-# LANGUAGE ViewPatterns #-}

module Lib
    ( someFunc
    , Point
    , LineSegment
    , distance
    , perpendicularDistance
    , splitAtMaxDistance
    , douglasPeucker
    ) where

import           Data.Sequence as D

type Point = (Double,Double)
type LineSegment = (Point,Point)

distance :: Point -> Point -> Double
distance (x1,y1) (x2,y2) = sqrt(((x1 - x2) ^ 2) + ((y1 - y2) ^ 2))

-- http://paulbourke.net/geometry/pointlineplane/DistancePoint.java
perpendicularDistance :: Point -> LineSegment -> Double
perpendicularDistance p@(pX, pY) (a@(aX, aY), b@(bX, bY))
    | a == b = distance p a
    | u < 0 = distance p a
    | u > 1 = distance p b
    | otherwise = distance p (aX + u * deltaX, aY + u * deltaY)
    where
        (deltaX, deltaY) = (bX - aX, bY - aY)
        u = ((pX - aX) * deltaX + (pY - aY) * deltaY) / (deltaX * deltaX + deltaY * deltaY)

-- https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
douglasPeucker :: Double -> Seq Point -> Seq Point
douglasPeucker epsilon points
  | points == D.empty = D.empty
  | dmax >= epsilon = allButLast(douglasPeucker epsilon left) >< douglasPeucker epsilon right
  | otherwise = first points <| Lib.last points <| D.empty
  where
      (left, right) = D.splitAt (index + 1) points
      (dmax, index) = splitAtMaxDistance points

-- Takes a sequence of points but ignores the first and last elements.
splitAtMaxDistance :: Seq Point -> (Double, Int)
splitAtMaxDistance points =
    D.foldlWithIndex (\(max, index) ni a -> if cp a ls > max then (cp a ls, ni) else (max, index)) (0.0, 0) (modPts points)
    where
        ls = (first points, Lib.last points)
        cp = perpendicularDistance
        modPts points = allButLast $ D.drop 1 points

last :: Seq a -> a
last (viewr -> xs :> x) = x

allButLast :: Seq a -> Seq a
allButLast (viewr -> xs :> x) = xs
allButLast (viewr -> xs) = empty

first :: Seq a -> a
first (viewl -> x :< xs) = x

someFunc :: IO ()
someFunc = putStrLn "someFunc"
