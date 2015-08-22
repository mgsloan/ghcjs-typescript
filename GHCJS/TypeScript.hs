{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

-- Comments prefixed "TSS:" come from the TypeScript Spec 1.5
module GHCJS.TypeScript where

import Prelude hiding (String)
import Data.Proxy
import Data.Coerce
import GHC.TypeLits
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Prim
import Data.Typeable
import Data.Type.Bool
import Data.Type.Equality

newtype Any = Any (JSRef Any)
  deriving (Typeable, ToJSRef, FromJSRef)

newtype Undefined = Undefined (JSRef Undefined)
  deriving (Typeable, ToJSRef, FromJSRef)

newtype Void = Void (JSRef Void)
  deriving (Typeable, ToJSRef, FromJSRef)

newtype Null = Null (JSRef Null)
  deriving (Typeable, ToJSRef, FromJSRef)

newtype Number = Number (JSRef Number)
  deriving (Typeable, ToJSRef, FromJSRef)

newtype Boolean = Boolean (JSRef Boolean)
  deriving (Typeable, ToJSRef, FromJSRef)

newtype String = String (JSRef String)
  deriving (Typeable, ToJSRef, FromJSRef)

newtype Array a = Array (JSRef (Array a))
  deriving (Typeable, ToJSRef, FromJSRef)

newtype Object (fs :: [(Symbol, *)]) = Object (JSRef (Object fs))
  deriving (Typeable, ToJSRef, FromJSRef)

newtype Optional a = Optional a
  deriving (Typeable, ToJSRef, FromJSRef)

newtype (:|:) a b = Union (JSRef (a :|: b))
  deriving (Typeable, ToJSRef, FromJSRef)

type family Members a :: [(Symbol, *)]
-- TSS: The apparent members of the primitive types Number, Boolean, and String
-- are the apparent members of the global interface types 'Number', 'Boolean',
-- and 'String' respectively.
--
-- FIXME: down the road, maybe these should be generated.  For now pretend like
-- they're opaque.
type instance Members Number = '[]
type instance Members Boolean = '[]
type instance Members String = '[]
type instance Members (Array a) = '[ '("length", Number) ]
-- Members of an Object are its fields.
type instance Members (Object fs) = fs
-- TSS: If a type is not one of the above, it is considered to have no apparent
-- members.
type instance Members Any = '[]
type instance Members Undefined = '[]
type instance Members Null = '[]
-- TODO: Generate with TH
type instance Members (a, b) = '[ '("0", a), '("1", b) ]
type instance Members (a, b, c) = '[ '("0", a), '("1", b), '("2", c) ]
type instance Members (a, b, c, d) = '[ '("0", a), '("1", b), '("2", c), '("3", d) ]
-- The apparent members of a union type U are determined as follows:
-- * If each type in U has an apparent property P, U has an apparent property P of a union
--   type of the types of P from each type in U.
-- FIXME: handle the rest
type instance Members (a :|: b) = UnionMembers (Members a) (Members b)

type family LookupMember (k :: Symbol) (fs :: [(Symbol, *)]) :: Maybe * where
  LookupMember k '[] = 'Nothing
  LookupMember k ('(k, x) ': xs) = 'Just x
  LookupMember k ('(kother, x) ': xs) = LookupMember k xs

type GetMember k fs = FromMaybe (GetMemberNotFound k) (LookupMember k fs)
data GetMemberNotFound :: Symbol -> *

type family FromMaybe d m where
  FromMaybe d 'Nothing = d
  FromMaybe d ('Just x) = x

type (<:) s t = ((s <:? t) ~ 'True)

-- In the TSS this is actually called assignment compatibility - subtyping is
-- subtly different.
--
-- Following comments are all from TSS.
type family (<:?) s t where
  -- Not TSS: This must fail, so that 'ObjectSubtype' works correctly for
  -- missing fields.
  (<:?) (GetMemberNotFound msg) t = 'False
  -- S and T are identical types.
  (<:?) s s = 'True
  -- S or T is the Any type.
  (<:?) Any t = 'True
  (<:?) s Any = 'True
  -- S is the Undefined type.
  (<:?) Undefined t = 'True
  -- S is the Null type and T is not the Undefined type.
  (<:?) Null Undefined = 'False
  (<:?) Null t = 'True
  -- FIXME S is an enum type and T is the primitive type Number.
  -- (<:?) Enum Number = True
  --
  -- FIXME S is a string literal type and T is the primitive type String.
  -- (<:?) s String = IsStringType s
  --
  -- FIXME S and T are type parameters, and S is directly or indirectly constrained to T.
  --
  -- S is a union type and each constituent type of S is assignable to T.
  (<:?) (s1 :|: s2) t = (s1 <:? t) && (s2 <:? t)
  -- T is a union type and S is assignable to at least one constituent type of T.
  (<:?) s (t1 :|: t2) = (s <:? t1) || (s <:? t2)
  -- Not TSS: Only matching primitive types are subtypes of primitive
  -- types. (well, except (Enum <: Number))
  (<:?) s Number = 'False
  (<:?) s Boolean = 'False
  (<:?) s String = 'False
  -- S is an object type, a type parameter, or the Number, Boolean, or String
  -- primitive type, T is an object type, and for each member M in T, one of
  -- the following is true:
  (<:?) s t = ObjectAssignable (Members s) (Members t)

type family ObjectAssignable ns ms where
  -- * M is a property and S has an apparent property N where
  --   * M and N have the same name,
  --   * the type of N is assignable to that of M,
  --   * if M is a required property, N is also a required property, and
  -- TODO: consider if we want to be concerned with visibility modifiers?
  --   * M and N are both public, M and N are both private and originate in the
  --     same declaration, M and N are both protected and originate in the same
  --     declaration, or M is protected and N is declared in a class derived
  --     from the class in which M is declared.
  -- * M is an optional property and S has no apparent property of the same name as M.
  ObjectAssignable ns '[] = 'True
  ObjectAssignable ns ('(k, m) ': ms) =
    (If (IsOptional m)
        'True
        (GetMember k ns <:? m)
    ) && ObjectAssignable ns ms

type family IsOptional a where
  IsOptional (Optional a) = 'True
  IsOptional a = 'False

type family UnionMembers (ns :: [(Symbol, *)]) (ms :: [(Symbol, *)]) where
  UnionMembers '[] ms = '[]
  UnionMembers ns '[] = '[]
  UnionMembers ('(k, n) ': ns) ms = UnionMembers' k n ns ms (LookupMember k ms)

type family UnionMembers' k n ns ms mm where
  UnionMembers' k n ns ms 'Nothing = UnionMembers ns ms
  UnionMembers' k n ns ms ('Just m) = '(k, ExtendUnion n m) ': UnionMembers ns ms

-- Utility which attempts to keep union types small when extending them.
-- Assumes unions are left-associated.
type ExtendUnion x u = If (x <:? u) u (RemoveSubtypes x u :|: x)

-- FIXME: determine if it matters that this is using assignability and not
-- subtyping (see section 3.4)
type family RemoveSubtypes x xs where
  RemoveSubtypes x (u :|: y) = If (y <:? x) u (u :|: y)
  RemoveSubtypes x u = u

-- Functions

tsCast :: (s <: t) => JSRef s -> JSRef t
tsCast = coerce

toObject :: Coercible (JSRef a) (Object (Members a))
         => JSRef a
         -> Object (Members a)
toObject = coerce

fromObject :: (Coercible (Object fs) b, b <: Object fs) => Object fs -> b
fromObject = coerce

getMember :: KnownSymbol k
          => JSRef a
          -> Proxy k
          -> IO (JSRef (GetMember k (Members a)))
getMember x = getProp x . symbolVal

-- objectAdd :: (LookupMember k fs ~ Nothing) => Proxy k -> JSRef t -> Object fs -> IO (Object ('(k, t) ': fs))
-- objectAdd k x obj = do
--   setProp (symbolVal k) x obj
--   return (coerce obj)

-- objectInsert :: Proxy k -> JSRef t -> Object fs -> IO (Object (Insert k t fs))
-- objectInsert k x obj = do
--   setProp (symbolVal k) x obj
--   return (coerce obj)

type family Insert k t fs where
  Insert k t fs =
    If (LookupMember k fs == 'Nothing)
       ('(k, t) ': fs)
       fs
