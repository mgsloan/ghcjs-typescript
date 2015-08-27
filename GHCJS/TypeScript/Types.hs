{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE PolyKinds                  #-}
module GHCJS.TypeScript.Types where

import Data.Coerce
import Data.Type.Bool
import Data.Type.Equality
import Data.Typeable
import GHC.TypeLits
import GHCJS.Marshal
import GHCJS.Prim
import Prelude hiding (String)

--------------------------------------------------------------------------------
-- Members

-- | Label on an object member.
data Label
  = Property Symbol
  | Call
  | Constructor
  | StringIndex
  | NumericIndex

-- | Gets the list of apparent members for a type.
type family Members a :: [(Label, *)]

type instance Members (JSRef a) = Members a

-- | Constraint used to get the type of the specified property.
-- Finally, provides the KnownSymbol k constraint.
type HasProperty obj k t =
  ( HasMember obj ('Property k) t
  , KnownSymbol k
  )

-- | Constraint used to get the type of the specified member.
--   Also constrains that obj and t are coercible with JSRef, as this is
--   always the case.
type HasMember obj k t =
  ( StripOptional (LookupMember k (Members obj)) ~ Just t
  , IsJSRef obj
  , IsJSRef t
  )

type IsJSRef a = Coercible a (JSRef O)
data O

type family StripOptional t where
  StripOptional ('Just (Optional t)) = 'Just t
  StripOptional t = t

-- | Given a 'Label', looks up the type of the member.  Yields
-- 'Nothing' if it's not found.
type family LookupMember (k :: Label) (fs :: [(Label, *)]) :: Maybe * where
  LookupMember k '[] = 'Nothing
  LookupMember k ('(k, x) ': xs) = 'Just x
  LookupMember k ('(kother, x) ': xs) = LookupMember k xs

-- | Given a 'Label', looks up the type of the member, and removes it.
-- Yields 'Nothing' if it's not found.
type family LookupAndRemoveMember (k :: Label)
                                  (fs :: [(Label, *)])
                                  :: Maybe (*, [(Label, *)]) where
  LookupAndRemoveMember k '[] = 'Nothing
  LookupAndRemoveMember k ('(k, x) ': xs) = 'Just '(x, xs)
  LookupAndRemoveMember k ('(kother, x) ': xs) =
    AppendToJustSnd '(kother, x) (LookupAndRemoveMember k xs)

type family AppendToJustSnd (x :: a)
                            (xs :: Maybe (*, [a]))
                            :: Maybe (*, [a]) where
  AppendToJustSnd x 'Nothing = 'Nothing
  AppendToJustSnd x ('Just '(t, y)) = 'Just '(t, x ': y)


type family FromMaybe d m where
  FromMaybe d 'Nothing = d
  FromMaybe d ('Just x) = x

-- | Type family with no instances, to provide compiletime diagnostics.
type family Error (x :: k1) :: k2

-- | When this is wrapped around a property type, indicates that it is
-- optional.
newtype Optional a = Optional a
  deriving (Typeable, ToJSRef, FromJSRef)

-- | Convenient way to declare a method.
type Method l f = '( 'Property l, Object '[ '( 'Call, f ) ] )

-- | Convenient way to declare an optional method.
type OptionalMethod l f = '( 'Property l, Optional (Object '[ '( 'Call, f ) ]) )

--------------------------------------------------------------------------------
-- Primitives / built in types

newtype Any       = Any       (JSRef Any)       deriving (Typeable, ToJSRef, FromJSRef)
newtype Undefined = Undefined (JSRef Undefined) deriving (Typeable, ToJSRef, FromJSRef)
newtype Void      = Void      (JSRef Void)      deriving (Typeable, ToJSRef, FromJSRef)
newtype Null      = Null      (JSRef Null)      deriving (Typeable, ToJSRef, FromJSRef)
newtype Number    = Number    (JSRef Number)    deriving (Typeable, ToJSRef, FromJSRef)
newtype Boolean   = Boolean   (JSRef Boolean)   deriving (Typeable, ToJSRef, FromJSRef)
newtype String    = String    (JSRef String)    deriving (Typeable, ToJSRef, FromJSRef)
newtype Array a   = Array     (JSRef (Array a)) deriving (Typeable, ToJSRef, FromJSRef)

-- FIXME TSS(3.10.1): The apparent members of the primitive types
-- Number, Boolean, and String are the apparent members of the global
-- interface types 'Number', 'Boolean', and 'String' respectively.

-- TSS(3.10.1): If a type is not one of the above, it is considered to
-- have no apparent members.
type instance Members Any = '[]
type instance Members Undefined = '[]
type instance Members Null = '[]
type instance Members (Array a) = '[ '( 'NumericIndex, a ) ]

--------------------------------------------------------------------------------
-- Object

newtype Object (fs :: [(Label, *)]) = Object (JSRef (Object fs))
  deriving (Typeable, ToJSRef, FromJSRef)

type instance Members (Object fs) = fs

--------------------------------------------------------------------------------
-- Union

newtype a :|: b = Union (JSRef (a :|: b))
  deriving (Typeable, ToJSRef, FromJSRef)

-- TSS: The apparent members of a union type U are determined as follows:
--
-- * If each type in U has an apparent property P, U has an apparent
-- property P of a union type of the types of P from each type in U.
-- FIXME: handle the rest
type instance Members (a :|: b) = UnionMembers (Members a) (Members b)

type family UnionMembers (ns :: [(Label, *)]) (ms :: [(Label, *)]) where
  UnionMembers '[] ms = '[]
  UnionMembers ns '[] = '[]
  UnionMembers ('(k, n) ': ns) ms = UnionMembers' k n ns ms (LookupMember k ms)

type family UnionMembers' k n ns ms mm where
  UnionMembers' k n ns ms 'Nothing = UnionMembers ns ms
  UnionMembers' k n ns ms ('Just m) = '(k, AddToUnion n m) ': UnionMembers ns ms

-- Utility which attempts to keep union types small when extending them.
-- Assumes unions are left-associated.
type AddToUnion x u = If (x <:? u) u (RemoveSubtypes x u :|: x)

type family RemoveSubtypes x xs where
  RemoveSubtypes x (u :|: y) = If (y <:? x) u (u :|: y)
  RemoveSubtypes x u = u

--------------------------------------------------------------------------------
-- Type Relationships

type a <: b = ((a <:? b) ~ 'True, IsJSRef a, IsJSRef b)
type a := b = ((a :=? b) ~ 'True, IsJSRef a, IsJSRef b)
type a <:? b = Rel a 'SubtypeOf b
type a :=? b = Rel a 'AssignableTo b

data Relationship
  = SubtypeOf
  | AssignableTo

type family Rel s (r :: Relationship) t where
  -- TSS(3.10.3/4): S and T are identical types.
  Rel s r s = 'True
  -- TSS(3.10.3 - Subtype): T is the Any type.
  -- TSS(3.10.4 - Assignable): S or T is the Any type.
  Rel Any 'AssignableTo t = 'True
  Rel s r Any = 'True
  -- TSS(3.10.3/4): S is the Undefined type.
  Rel Undefined r t = 'True
  -- TSS(3.10.3/4): S is the Null type and T is not the Undefined
  -- type.
  Rel Null r Undefined = 'False
  Rel Null r t = 'True
  -- FIXME TSS(3.10.4): S is an enum type and T is the primitive type
  -- Number.
  --
  -- Rel AssignableTo Enum Number = 'True
  --
  -- FIXME TSS(3.10.3/4): S is a string literal type and T is the
  -- primitive type String.
  --
  -- TSS(3.10.3/4): S is a union type and each constituent type of S is
  -- a subtype of / assignable to T.
  Rel (s1 :|: s2) r t = (Rel s1 r t) && (Rel s2 r t)
  -- TSS(3.10.3/4): T is a union type and S is a subtype of /
  -- assignable to at least one constituent type of T.
  Rel s r (t1 :|: t2) = (Rel s r t1) || (Rel s r t2)
  -- Only matching primitive types are subtypes of primitive types.
  Rel s r Number = 'False
  Rel s r Boolean = 'False
  Rel s r String = 'False
  -- TSS (3.10.3/4): S is an object type, a type parameter, or the
  -- Number, Boolean, or String primitive type, T is an object type,
  -- and for each member M in T, one of the following is true:
  Rel s r t = ObjectRel (Members s) r (Members t)

type family ObjectRel r ns ms where
  ObjectRel ns r '[] = 'True
  ObjectRel ns r ('(k, m) ': ms) =
    MemberRel k (LookupMember k ns) r m && ObjectRel ns r ms

type family MemberRel k r n m where
  -- TSS (3.10.4 - Assignable):
  -- M is an optional property and S has no apparent property of the
  -- same name as M.
  MemberRel ('Property k) 'Nothing             'AssignableTo (Optional m) = 'True
  -- (the above case is the only circumstance where it's ok for the N
  -- to be 'Nothing)
  MemberRel k             'Nothing             r             m            = 'False
  -- TSS (3.10.3/4):
  -- M is a property and S has an apparent property N where
  --   * M and N have the same name,
  --   * the type of N is assignable to / subtype of M,
  MemberRel ('Property k) ('Just (Optional n)) r             (Optional m) = Rel n r m
  --   * if M is a required property, N is also a required property
  MemberRel ('Property k) ('Just (Optional n)) r             m            = 'False
  MemberRel ('Property k) ('Just n)            r             (Optional m) = Rel n r m
  MemberRel ('Property k) ('Just n)            r             m            = Rel n r m
  -- TSS (3.10.3/4):
  -- FIXME: figure out whether we can handle generics in this way...
  --
  -- M is a non-specialized call or construct signature and S has an
  -- apparent call or construct signature N where, when M and N are
  -- instantiated using type Any as the type argument for all type
  -- parameters declared by M and N (if any),
  --
  --   * the signatures are of the same kind (call or construct),
  MemberRel 'Call         ('Just n)            r             m            = CallRel n r m
  MemberRel 'Constructor  ('Just n)            r             m            = CallRel n r m
  -- TSS (3.10.3/4): M is a string index signature of type U and S has
  -- an apparent string index signature of a type that is assignable
  -- to / subtype of U.
  MemberRel 'StringIndex  ('Just n)            r             m            = Rel n r m
  MemberRel 'NumericIndex ('Just n)            r             m            = Rel n r m

type family CallRel n r m where
  -- FIXME; handle rest parameter
  --
  --   * M has a rest parameter or the number of non-optional
  --     parameters in N is less than or equal to the total number of
  --     parameters in M,
  --
  --   * for parameter positions that are present in both signatures,
  --     each parameter type in N is assignable to or from the
  --     corresponding parameter type in M, and
  --
  --   * the result type of M is Void, or the result type of N is
  --     assignable to that of M.
  CallRel (np -> nr) r (mp -> mr) =
      (  Rel (UnOptional np) r (UnOptional mp)
      || Rel (UnOptional mp) r (UnOptional np)
      ) && CallRel nr r mr
  CallRel n r m = CallRel' n r m

type family CallRel' n r m where
  CallRel' (Optional np -> nr) r m = CallRel' nr r m
  CallRel' (np -> nr) r m = 'False
  CallRel' n r (mp -> mr) = CallRel' n r mr
  CallRel' n r m = Rel n r m

type family UnOptional a where
  UnOptional (Optional a) = a
  UnOptional a = a

-- List of rules I'm ignoring as they're irrelevant to this implementation:
--
-- * Field visibility (TSS(3.10.3) M and N are both public, M and N
-- are both private and originate in the...)
--
--   - TSS(7.1) An interface cannot declare a property with the same
--     name as an inherited private or protected property.
--
-- * Type parameters (TSS(3.10.3) S and T are type parameters, and S
--   is directly or indirectly constrained to T...)
--
-- * TSS(7.1) An interface declaration may not, directly or
-- indirectly, specify a base type that originates in the same
-- declaration. In other words an interface cannot, directly or
-- indirectly, be a base type of itself, regardless of type arguments

--------------------------------------------------------------------------------
-- Extension

-- Quotes from TSS(7.1):
--
-- > An interface has the members specified in the ObjectType of its
-- > declaration and furthermore inherits all base type members that
-- > aren't hidden by declarations in the interface.
--
-- > Inherited properties with the same name must be identical.
--
-- Members declared in the interface which shadow inherited interfaces
-- must be subtypes or assignable.  There is some confusion on this
-- topic, though, because, in a section about classes:
--
-- > TSS(8.2.3) The type of an overriding property member must be
-- > assignable (section 3.10.4) to the type of the overridden property
-- > member, or otherwise a compile-time error occurs
--
-- On the other hand, the section on interfaces has the following
-- example:
--
-- > interface Mover {
-- >  move(): void;
-- >  getStatus(): { speed: number; };
-- > }
-- >
-- > interface Shaker {
-- >  shake(): void;
-- >  getStatus(): { frequency: number; };
-- > }
-- >
-- > An interface that extends 'Mover' and 'Shaker' must declare a new
-- > 'getStatus' property as it would otherwise inherit two 'getStatus'
-- > properties with different types. The new 'getStatus' property must
-- > be declared such that the resulting 'MoverShaker' is a **subtype** <<
-- > of both 'Mover' and 'Shaker':
--
-- > interface MoverShaker extends Mover, Shaker {
-- >  getStatus(): { speed: number; frequency: number; };
-- > }
--
-- Anyway, aside from whether we use subtyping or assignability,
-- AFAICT, this means that we implement interface extension by:
--
-- 1) Checking that the names shadowed in the inherited definitions
-- are subtypes / assignable.
--
-- 2) Remove all of these shadowed names from the inherited
-- definitions lists.
--
-- 3) Combine the resulting definition lists, requring that matching
-- field names have identical types.
--
-- 4) Prepend the definitions from the interface.

-- | Implements the 'extends' clause of interface definition.
type Extends objs ms =
  (CombineAllAndCheckIdentical
    (CheckAndRemoveShadowedMembers ms objs))

type family CheckAndRemoveShadowedMembers ms objs where
  CheckAndRemoveShadowedMembers ms '[] = '[]
  CheckAndRemoveShadowedMembers ms (obj ': objs) =
    CheckAndRemoveShadoweds ms (Members obj) ': CheckAndRemoveShadowedMembers ms objs

type family CheckAndRemoveShadoweds ms ns where
  CheckAndRemoveShadoweds '[] ns = ns
  CheckAndRemoveShadoweds ('(k, m) ': ms) ns =
    CheckAndRemoveShadowed
      k
      m
      ns
      (LookupAndRemoveMember k ns)

type family CheckAndRemoveShadowed (k :: Label)
                                   (m :: *)
                                   (ns :: [(Label, *)])
                                   (mn :: Maybe (*, [(Label, *)]))
                                   :: [(Label, *)] where
  CheckAndRemoveShadowed k m ns 'Nothing = ns
  CheckAndRemoveShadowed k m oldns ('Just '(n, ns)) =
    -- Use assignability (See notes above about whether this ought to
    -- be subtyping)
    If (m :=? n)
       ns
       (Error '( "When shadowing interfaces in Extends, field"
               , k
               , "::"
               , m
               , "should be assignable to the type of the field in the superclass"
               , n))

type family CombineAllAndCheckIdentical (xs :: [[(Label, *)]]) :: [(Label, *)] where
  CombineAllAndCheckIdentical '[] = '[]
  CombineAllAndCheckIdentical '[ms] = ms
  CombineAllAndCheckIdentical (ms ': ns ': xs) =
    CombineAllAndCheckIdentical (CombineAndCheckIdentical ms ns ': xs)

type family CombineAndCheckIdentical ms ns where
  CombineAndCheckIdentical '[] ns = ns
  CombineAndCheckIdentical ('(k, m) ': ms) ns =
    '(k, m) ':
    CombineAndCheckIdentical ms
      (CheckIdentical k m ns (LookupAndRemoveMember k ns))

type family CheckIdentical (k :: Label)
                           (m :: *)
                           (ns :: [(Label, *)])
                           (x :: Maybe (*, [(Label, *)]))
                           :: [(Label, *)] where
  CheckIdentical k m ns 'Nothing = ns
  CheckIdentical k m ns ('Just '(n, newns)) =
    If (m == n)
       newns
       (Error '( "When combining interfaces in Extends, expected field"
               , k
               , "to have identical types, but instead they are"
               , m
               , "and"
               , n))

-- Points to revisit:
--
-- * All properties of the interface must satisfy the constraints
-- implied by the index signatures of the interface as specified in
-- section 3.8.4.
--
-- * The instance type (section 3.6.1) of the declared interface must
-- be assignable (section 3.10.4) to each of the base type references.
--
--   - NOTE It seems intuitive to me that the rules of extension and
--   assignability guarantee this, so it doesn't need to be checked.
