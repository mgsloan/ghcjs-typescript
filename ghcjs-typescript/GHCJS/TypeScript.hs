{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module GHCJS.TypeScript
  (
  -- * Casting types
    cast
  -- * Accessing members
  , P(..)
  , getProperty
  , getIndex
  , getStringIndex
  -- * Invoking functions and constructors
  , call
  , apply
  , callConstructor
  , construct
  -- * Anonymous objects
  , Obj(..)
  , newObj
  , toObj
  , objSet
  , objAdd
  -- * Unions
  , (:|:)(..)
  -- * Defining members
  , Members
  , Label(..)
  , type (:::)
  , type (::?)
  , Fun
  , Call
  , Constructor
  , StringIndex
  , NumericIndex
  , Extends
  -- * Constraints on types
  , IsJSRef
  , HasProperty
  , HasMember
  , HasCall
  , HasConstructor
  , type (<:)
  , type (:=)
  -- * Built-in types / primitives
  , Any(..)
  , Undefined(..)
  , Void(..)
  , Null(..)
  , Number(..)
  , Boolean(..)
  , String(..)
  , Array(..)
  -- * Types which appear in error messages
  , Error
  , LookupMember
  , InsertMember
  , InvokeConstructor
  , InvokeResult
  , GetMemberResult(..)
  , RelResult(..)
  , Relationship(..)
  , Optional
  , Rest
  , Specialize
  -- * Misc utils
  , toString
  , listToArray
  ) where

import           Data.Coerce
import           Data.HVect
import           Data.Type.Bool
import           Data.Type.Equality (type (==))
import           Data.Typeable (Typeable)
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           GHCJS.Marshal (ToJSRef, FromJSRef)
import           GHCJS.Prim (JSRef(..), getProp, toJSArray, toJSString)
import           GHCJS.TypeScript.Types
import qualified Prelude (String)
import           Prelude hiding (String)

--------------------------------------------------------------------------------
-- Casting types

cast :: (t := s) => s -> t
cast = coerce

--------------------------------------------------------------------------------
-- Accessing members

-- | Use this to pass type-level strings in to functions.  It's the
-- same thing as @Data.Proxy.Proxy@, but with a shorter name.
data P (t :: k) = P

-- FIXME: Variants that handle optional instead of having
-- 'StripOptional' in 'HasMember'

getProperty :: HasProperty obj k r => p k -> obj -> IO r
getProperty k x = coerce (getProp (coerce x) (symbolVal k))

getIndex :: HasMember obj 'NumericIndex r => Number -> obj -> IO (JSRef r)
getIndex ix x = js_index (coerce x) (coerce ix)

getStringIndex :: HasMember obj 'StringIndex r => String -> obj -> IO r
getStringIndex ix x = coerce (js_index (coerce x) (coerce ix))

foreign import javascript unsafe "$1[$2]" js_index :: JSRef obj -> JSRef i -> IO (JSRef r)

--------------------------------------------------------------------------------
-- Invoking functions and constructors

-- | Call an object's method with the given arguments.
call :: (HasProperty obj k func, HasCall func ts ty r)
     => p k -> HVect ts -> obj -> IO r
call k args obj = do
  func <- getProperty k obj
  apply func obj args

-- | Invokes the passed function object, with the given arguments.
--
-- NOTE: this isn't safe because it doesn't constrain the 'this' type.
apply :: (HasCall func ts ty r, IsJSRef this)
      => func -> this -> HVect ts -> IO r
apply func this args = do
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

-- | Lookup a constructor in an object, and run it.
callConstructor :: (HasProperty obj k func, HasConstructor func ts ty r)
                => p k -> HVect ts -> obj -> IO r
callConstructor k args obj = do
  func <- getProperty k obj
  construct func args

-- | Invokes the passed constructor object, with the given arguments.
construct :: (HasConstructor obj ts ty r)
          => obj -> HVect ts -> IO r
construct proto = coerce . invokeConstructorImpl (coerce proto)

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

--------------------------------------------------------------------------------
-- Anonymous Objects

newtype Obj (fs :: [(Label, *)]) = Obj (JSRef (Obj fs))
  deriving (Typeable, ToJSRef, FromJSRef)

type instance Members (Obj fs) = fs

-- | Creates a new empty object.
newObj :: IO (Obj '[])
newObj = coerce js_emptyObj
foreign import javascript unsafe "$r = {};" js_emptyObj :: IO (JSRef a)

-- | Converts an object to its anonymous representation.
toObj :: IsJSRef obj => obj -> Obj (Members obj)
toObj = coerce

-- | Sets a property on the object.  If the property is already set,
-- then this replaces it.
objSet :: (KnownSymbol k, IsJSRef t)
       => p k -> t -> Obj fs -> IO (Obj (InsertMember ('Property k) t fs))
objSet k x obj = do
  js_setProp (toJSString (symbolVal k)) (coerce x) (coerce obj)
  return (coerce obj)

-- | Sets a property on the object, but only if it isn't already in
-- the object's field list.  Note this is only a static check, no
-- runtime checks are performed.
objAdd :: (LookupMember ('Property k) fs ~ Nothing, KnownSymbol k, IsJSRef t)
       => p k -> t -> Obj fs -> IO (Obj ('( 'Property k, t ) ': fs))
objAdd k x obj = do
  js_setProp (toJSString (symbolVal k)) (coerce x) (coerce obj)
  return (coerce obj)

foreign import javascript unsafe "$3[$1] = $2"
  js_setProp :: JSRef str -> JSRef a -> JSRef b -> IO ()

type family InsertMember k t fs where
  InsertMember k t fs =
    If (LookupMember k fs == 'Nothing)
       ('(k, t) ': fs)
       fs

--------------------------------------------------------------------------------
-- Defining object members

-- | Convenient way to declare a property.
type l ::: t = '( 'Property l, t )

-- | Convenient way to declare an optional property.
type l ::? t = '( 'Property l, Optional t )

-- | Convenient way to specify the function call member.
type Call t = '( 'Call, t )

-- | Convenient way to specify the constructor member.
type Constructor t = '( 'Constructor, t )

-- | Convenient way to specify the string index member.
type StringIndex t = '( 'StringIndex, t )

-- | Convenient way to specify the numeric index member.
type NumericIndex l t = '( 'NumericIndex, t )

-- | Type synonym for an anonymous object which can be called.
type Fun f = Obj '[Call f]

--------------------------------------------------------------------------------
-- Arrays

-- | Array object which supports numeric indexing.
newtype Array a = Array (JSRef (Array a))
  deriving (Typeable, ToJSRef, FromJSRef)

type instance Members (Array a) = '[ '( 'NumericIndex, a ) ]

--------------------------------------------------------------------------------
-- Misc utils

toString :: Prelude.String -> String
toString = coerce . toJSString

listToArray :: IsJSRef a => [a] -> IO (Array a)
listToArray = coerce . toJSArray . coerce
