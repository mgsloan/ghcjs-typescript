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
  ( LookupMember 'Call (Members obj) ~ 'Just ty
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
invokeConstructor func args = do
  argsList <- toJSArray (hvectToRefList args)
  coerce (js_construct (coerce func) argsList)

foreign import javascript unsafe "$1.bind.apply($1, $2)"
   js_construct :: JSRef proto -> JSRef args -> IO (JSRef result)

type HasConstructor obj ts ty r =
  ( LookupMember 'Constructor (Members obj) ~ 'Just ty
  , HVectToRefList (HVect ts)
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
