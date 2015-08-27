{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

import GHCJS.TypeScript.Types
import GHCJS.TypeScript.Object

import Data.Type.Bool
import Data.Typeable hiding (cast)
import GHC.TypeLits
import GHCJS.Marshal
import GHCJS.Prim
import Prelude hiding (String)

-- Ex1

newtype T1 = T1 (JSRef T1)
  deriving (ToJSRef, FromJSRef)

type instance Members T1 = '[ '(Property "label", String) ]

getLabel :: T1 -> IO String
getLabel = getProperty (P::P"label")

-- Ex2

newtype T2 = T2 (JSRef T2)
  deriving (ToJSRef, FromJSRef)

type instance Members T2 =
  '[ '(Property "label", String)
   , '(Property "value", Number)
   ]

testCasting1 :: JSRef T2 -> JSRef T1
testCasting1 = cast

-- Ex3

newtype T3 = T3 (JSRef T3)
  deriving (ToJSRef, FromJSRef)

type instance Members T3 =
  '[ '(Property "label", String)
   , '(Property "value", Optional Number)
   ]

testCasting2 :: JSRef T3 -> JSRef T1
testCasting2 = cast

testCasting3 :: JSRef T1 -> JSRef T3
testCasting3 = cast

-- Ex4

testUnionMembers :: JSRef (T1 :|: T2) -> Object (Members T1)
testUnionMembers = toObject

main = putStrLn "Worked!"

-- Ex5

newtype T5 = T5 (JSRef T5)
  deriving (ToJSRef, FromJSRef)

type instance Members T5 =
  '[ '(Call, Object '[ '(Property "x", Number)] -> Number) ]

newtype T5' = T5' (JSRef T5')
  deriving (ToJSRef, FromJSRef)

type instance Members T5' =
  '[ '(Call, Object '[ '(Property "x", Number), '(Property "y", Number)] -> Number) ]

testFunctionCast :: JSRef T5 -> JSRef T5'
testFunctionCast = cast
