{-# LANGUAGE ViewPatterns #-}

module Lib
    ( someFunc
    , Point
    , LineSegment
    , distance
    , allButLast
    , perpendicularDistance
    , splitAtMaxDistance
    , douglasPeucker
    ) where

import           Data.Sequence as D

type Point = (Double,Double)
type LineSegment = (Point,Point)

distance :: Point -> Point -> Double
distance (x1,y1) (x2,y2) = sqrt(((x1 - x2) ^ 2) + ((y1 - y2) ^ 2))

douglasPeucker :: Double -> Seq Point -> Seq Point
douglasPeucker epsilon points
  | points == D.empty = D.empty
  | dmax > epsilon = allButLast(douglasPeucker epsilon left) >< douglasPeucker epsilon right
  | otherwise = first points <| Lib.last points <| D.empty
  where
      (left, right) = D.splitAt index points
      (dmax, index) = splitAtMaxDistance (first points, Lib.last points) points

-- http://paulbourke.net/geometry/pointlineplane/DistancePoint.java
perpendicularDistance :: Point -> LineSegment -> Double
perpendicularDistance p@(pX, pY) (a@(aX, aY), b@(bX, bY))
    | u < 0 = distance p a
    | u > 1 = distance p b
    | otherwise = distance p (aX + u * deltaX, aY + u * deltaY)
    where
        (deltaX, deltaY) = (bX - aX, bY - aY)
        u = ((pX - aX) * deltaX + (pY - aY) * deltaY) / (deltaX * deltaX + deltaY * deltaY)

splitAtMaxDistance :: LineSegment -> Seq Point -> (Double, Int)
splitAtMaxDistance ls points =
    D.foldlWithIndex (\(max, index) ni a -> if cp a ls > max then (cp a ls, ni + 1) else (max, index)) (0.0, 0) (modPts points)
    where
        cp = perpendicularDistance
        modPts points = allButLast $ D.drop 1 points

last :: Seq a -> a
last (viewr -> xs :> x) = x

allButLast :: Seq a -> Seq a
allButLast (viewr -> xs :> x) = xs

first :: Seq a -> a
first (viewl -> x :< xs) = x

someFunc :: IO ()
someFunc = putStrLn "someFunc"
