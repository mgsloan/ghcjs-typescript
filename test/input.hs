{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Main where

import GHCJS.TypeScript as TS
import Data.Coerce
import GHCJS.Types (JSRef(..))
import GHCJS.Foreign

main = putStrLn "Hi!" -- unused

--------------------------------------------------------------------------------
-- Casting objects built from primitives

newtype XY = XY (JSRef XY)
type instance Members XY =
  '[ "x" ::: Number
   , "y" ::: Number
   ]

type XYZ = XYZ' Number
type XYOptionalZ = XYZ' (TS.Optional Number)
type XYStringZ = XYZ' TS.String

newtype XYZ' a = XYZ' (JSRef (XYZ' a))
type instance Members (XYZ' a) =
  '[ "x" ::: Number
   , "y" ::: Number
   , "z" ::: a
   ]

ex1 = undefined
  where
    ok0  = cast :: XY -> XY
    bad0 = cast :: XY -> XYZ
    ok1  = cast :: XYZ -> XY

    ok2  = cast :: XYOptionalZ -> XYOptionalZ
    ok3  = cast :: XY -> XYOptionalZ
    ok4  = cast :: XYZ -> XYOptionalZ
    bad1 = cast :: XYOptionalZ -> XYZ

    bad2 = cast :: XYZ -> XYStringZ
    bad3 = cast :: XYStringZ -> XYZ

--------------------------------------------------------------------------------
-- Casting objects with nested objects

newtype Circle = Circle (JSRef Circle)
type instance Members Circle =
  '[ "position" ::: XY
   , "radius" ::: Number
   ]

newtype Sphere = Sphere (JSRef Sphere)
type instance Members Sphere =
  '[ "position" ::: XYZ
   , "radius" ::: Number
   ]

ex2 = undefined
  where
    ok = cast :: Sphere -> Circle
    bad = cast :: Circle -> Sphere

--------------------------------------------------------------------------------
-- Casting functions

newtype OffsetCircle = OffsetCircle (JSRef OffsetCircle)
type instance Members OffsetCircle =
  '[ Call (XY -> Circle -> Circle) ]

newtype OffsetSphere = OffsetSphere (JSRef OffsetSphere)
type instance Members OffsetSphere =
  '[ Call (XYZ -> Sphere -> Sphere) ]

newtype MeasureCircle = MeasureCircle (JSRef MeasureCircle)
type instance Members MeasureCircle =
  '[ Call (Circle -> Number) ]

newtype MeasureSphere = MeasureSphere (JSRef MeasureSphere)
type instance Members MeasureSphere =
  '[ Call (Sphere -> Number) ]

ex3 = undefined
  where
    ok0 = cast :: OffsetSphere -> OffsetCircle
    bad0 = cast :: OffsetCircle -> OffsetSphere
    ok1 = cast :: MeasureCircle -> MeasureSphere
    ok2 = cast :: MeasureSphere -> MeasureCircle

--------------------------------------------------------------------------------
-- Casting union types

ex4 = undefined
  where
    ok0 = cast :: XY -> (XY :|: Sphere)
    ok1 = cast :: XY -> (Sphere :|: XY)
    ok3 = cast :: XYZ -> (XY :|: XYZ)
    ok4 = cast :: XYZ -> (XYZ :|: XY)
    ok5 = cast :: (XY :|: XYZ) -> XY
    ok6 = cast :: (XY :|: XYOptionalZ) -> XY
    ok7 = cast :: XY -> (Circle :|: Sphere :|: XY)
    bad0 = cast :: (XY :|: XYZ) -> XYZ
    bad1 = cast :: (XY :|: XYOptionalZ) -> XYZ
    bad2 = cast :: XY -> (Circle :|: Sphere :|: XYZ)

--------------------------------------------------------------------------------
-- Casting recursive types

newtype Linked = Linked (JSRef Linked)
type instance Members Linked  =
  '[ "next" ::: Linked ]

newtype LinkedA = LinkedA (JSRef LinkedA)
type instance Members LinkedA  =
  '[ "next" ::: LinkedB ]

newtype LinkedB = LinkedB (JSRef LinkedB)
type instance Members LinkedB  =
  '[ "next" ::: LinkedA ]

ex5 = undefined
  where
    ok0 = cast :: Linked -> LinkedA
    ok1 = cast :: LinkedA -> Linked
