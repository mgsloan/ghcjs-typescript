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

import GHCJS.TypeScript.Types as TS
import GHCJS.TypeScript.Object as TS
import GHCJS.TypeScript.Function as TS
import Data.Coerce
import GHCJS.Types (JSRef(..))
import GHCJS.Foreign

newtype XY = XY (JSRef XY)
type instance Members XY =
  ('[ '( 'TS.Property "x", Number )
    , '( 'TS.Property "y", Number )
    ])

newtype XYZ = XYZ (JSRef XYZ)
type instance Members XYZ =
  ('[ '( 'TS.Property "x", Number )
    , '( 'TS.Property "y", Number )
    , '( 'TS.Property "z", Number )
    ])

newtype XYMaybeZ = XYMaybeZ (JSRef XYMaybeZ)
type instance Members XYMaybeZ =
  ('[ '( 'TS.Property "x", Number )
    , '( 'TS.Property "y", Number )
    , '( 'TS.Property "z", TS.Optional Number )
    ])

unsafeEmpty :: IsJSRef a => IO a
unsafeEmpty = coerce newObject

main = putStrLn "Hi!" -- unused

xy :: XY
xy = undefined
xyz :: XYZ
xyz = undefined
xymz :: XYMaybeZ
xymz = undefined

bad_XYToXYZ = cast xy :: XYZ
ok_XYToXYMaybeZ = cast xy :: XYMaybeZ
ok_XYZToXY = cast xyz :: XY
ok_XYZToXYMaybeZ = cast xyz :: XYMaybeZ
bad_XYMaybeZToXYZ = cast xymz :: XYZ
