{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
module GHCJS.TypeScript.Function where

import Data.Coerce
import Data.HVect
import Data.Type.Bool
import GHCJS.Prim
import GHCJS.TypeScript.Types
import GHCJS.TypeScript.Object
import Prelude hiding (String)

invoke :: (HasProperty obj k func, HasCall func ts ty r)
       => P k -> HVect ts -> obj -> IO r
invoke k args obj = do
  func <- getProperty k obj
  invokeCall func obj args

-- NOTE: this isn't safe because it doesn't constrain the 'this' type.
invokeCall :: (HasCall obj ts ty r, IsJSRef this)
           => obj -> this -> HVect ts -> IO r
invokeCall func this args = do
  argsList <- toJSArray (hvectToRefList args)
  coerce (js_apply (coerce func) (coerce this) argsList)

foreign import javascript unsafe "$1.apply($2, $3)"
  js_apply :: JSRef func -> JSRef this -> JSRef args -> IO (JSRef result)

type HasCall obj ts ty r =
  ( GetMember 'Call obj ~ 'GetMemberSuccess ty
  , HVectToRefList (HVect ts)
  , InvokeResult ty ts ~ r
  , IsJSRef obj
  , IsJSRef r
  )

construct :: (HasProperty obj k func, HasConstructor func ts ty r)
          => P k -> HVect ts -> obj -> IO r
construct k args obj = do
  func <- getProperty k obj
  invokeConstructor func args

invokeConstructor :: (HasConstructor obj ts ty r)
                  => obj -> HVect ts -> IO r
invokeConstructor proto = coerce . invokeConstructorImpl (coerce proto)

-- No good way to invoke constructors with an array...
-- FIXME: generate with TH

class InvokeConstructor ts where
  invokeConstructorImpl :: JSRef proto -> HVect ts -> IO (JSRef result)

instance InvokeConstructor '[] where invokeConstructorImpl proto HNil = js_construct0 proto
foreign import javascript unsafe "new ($1)()"
  js_construct0 :: JSRef proto -> IO (JSRef result)

instance IsJSRef a0 => InvokeConstructor '[a0] where
  invokeConstructorImpl proto (a0 :&: HNil) =
    js_construct1 proto (coerce a0)
  invokeConstructorImpl _ _ = impossible
foreign import javascript unsafe "new ($1)($2)"
  js_construct1 :: JSRef proto -> JSRef a0 -> IO (JSRef result)

instance (IsJSRef a0, IsJSRef a1) => InvokeConstructor '[a0, a1] where
  invokeConstructorImpl proto (a0 :&: a1 :&: HNil) =
    js_construct2 proto (coerce a0) (coerce a1)
  invokeConstructorImpl _ _ = impossible
foreign import javascript unsafe "new $1($2, $3)"
  js_construct2 :: JSRef proto -> JSRef a0 -> JSRef a1 -> IO (JSRef result)

instance (IsJSRef a0, IsJSRef a1, IsJSRef a2) => InvokeConstructor '[a0, a1, a2] where
  invokeConstructorImpl proto (a0 :&: a1 :&: a2 :&: HNil) =
    js_construct3 proto (coerce a0) (coerce a1) (coerce a2)
  invokeConstructorImpl _ _ = impossible
foreign import javascript unsafe "new $1($2, $3, $4)"
  js_construct3 :: JSRef proto -> JSRef a0 -> JSRef a1 -> JSRef a2 -> IO (JSRef result)

impossible :: a
impossible = error "Impossible type in constructor invocation?!?"

type HasConstructor obj ts ty r =
  ( GetMember 'Constructor obj ~ 'GetMemberSuccess ty
  , InvokeConstructor ts
  , InvokeResult ty ts ~ r
  , IsJSRef obj
  , IsJSRef r
  )

class HVectToRefList a where
  hvectToRefList :: a -> [JSRef arr]

instance HVectToRefList (HVect '[]) where
  hvectToRefList HNil = []

instance ( IsJSRef t
         , HVectToRefList (HVect ts)
         ) => HVectToRefList (HVect (t ': ts)) where
  hvectToRefList (x :&: xs) = coerce x : hvectToRefList xs

type family InvokeResult (func :: *) (as :: [*]) :: * where
  InvokeResult (Optional p -> r) (a ': as) =
    If (p :=? a)
       (InvokeResult r as)
       (Error '("Argument of type ", a, "cannot be passed to parameter of type", p))
  InvokeResult (p -> r) (a ': as) =
    If (p :=? a)
       (InvokeResult r as)
       (Error '("Argument of type ", a, "cannot be passed to parameter of type", p))
  InvokeResult r '[] = InvokeResult' r
  InvokeResult r as = Error '("Excess arguments provided:", as)

type family InvokeResult' (func :: *) :: * where
  InvokeResult' (Optional p -> r) = InvokeResult' r
  InvokeResult' (p -> r) = Error '("Not enough arguments provided. Rest of function: ", p -> r)
  InvokeResult' r = r
