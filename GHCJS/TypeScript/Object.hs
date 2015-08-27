{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE PolyKinds                  #-}
module GHCJS.TypeScript.Object where

import Data.Coerce
import Data.Type.Equality
import Data.Type.Bool
import Data.Typeable
import GHC.TypeLits
import GHCJS.Marshal
import GHCJS.Prim
import GHCJS.TypeScript.Types
import Prelude hiding (String)
import qualified Prelude (String)

data P (t :: k) = P

newObject :: IO (Object '[])
newObject = coerce js_emptyObj

foreign import javascript unsafe "$r = {};" js_emptyObj :: IO (JSRef a)

cast :: (s := t) => s -> t
cast = coerce

toObject :: IsJSRef obj => obj -> Object (Members obj)
toObject = coerce

fromObject :: (obj := Object fs) => Object fs -> obj
fromObject = coerce

-- FIXME: if the type of the JSRef isn't demanded, this won't check it.

-- FIXME: Variants that handle optional instead of having 'StripOptional' in 'HasMember'

getProperty :: HasProperty obj k r => P k -> obj -> IO r
getProperty k x = coerce (getProp (coerce x) (symbolVal k))

getIndex :: HasMember obj 'NumericIndex r => Number -> obj -> IO (JSRef r)
getIndex ix x = js_index (coerce x) (coerce ix)

getStringIndex :: HasMember obj 'StringIndex r => String -> obj -> IO r
getStringIndex ix x = coerce (js_index (coerce x) (coerce ix))

foreign import javascript "$1[$2]" js_index :: JSRef obj -> JSRef i -> IO (JSRef r)

-- foreign import javascript "new $1()" unsafeConstruct :: JSRef a -> IO (JSRef b)
-- construct :: IO (JSRef (GetMember 'Constructor (Members a)))

objectAdd :: (LookupMember ('Property k) fs ~ Nothing, KnownSymbol k, IsJSRef t)
          => P k -> t -> Object fs -> IO (Object ('( 'Property k, t ) ': fs))
objectAdd k x obj = do
  js_unsafeSetProp (toJSString (symbolVal k)) (coerce x) (coerce obj)
  return (coerce obj)

objectInsert :: (KnownSymbol k, IsJSRef t)
             => P k -> t -> Object fs -> IO (Object (Insert ('Property k) t fs))
objectInsert k x obj = do
  js_unsafeSetProp (toJSString (symbolVal k)) (coerce x) (coerce obj)
  return (coerce obj)

foreign import javascript unsafe "$3[$1] = $2"
  js_unsafeSetProp :: JSRef str -> JSRef a -> JSRef b -> IO ()

type family Insert k t fs where
  Insert k t fs =
    If (LookupMember k fs == 'Nothing)
       ('(k, t) ': fs)
       fs

-- Misc utils

toString :: Prelude.String -> String
toString = coerce . toJSString

listToArray :: IsJSRef a => [a] -> IO (Array a)
listToArray = coerce . toJSArray . coerce
