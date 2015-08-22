import GHCJS.TypeScript

-- Ex1

newtype T1 = T1 (JSRef T1)
  deriving (ToJSRef, FromJSRef)

type instance Members T1 = '[ '("label", String) ]

getLabel :: T1 -> IO (JSRef String)
getLabel x = do
  ref <- toJSRef x
  getMember ref (Proxy :: Proxy "label")

-- Ex2

newtype T2 = T2 (JSRef T2)
  deriving (ToJSRef, FromJSRef)

type instance Members T2 =
  '[ '("label", String)
   , '("value", Number)
   ]

testCasting1 :: JSRef T2 -> JSRef T1
testCasting1 = tsCast

-- Ex3

newtype T3 = T3 (JSRef T3)
  deriving (ToJSRef, FromJSRef)

type instance Members T3 =
  '[ '("label", String)
   , '("value", Optional Number)
   ]

testCasting2 :: JSRef T3 -> JSRef T1
testCasting2 = tsCast

testCasting3 :: JSRef T1 -> JSRef T3
testCasting3 = tsCast

-- Ex4

testUnionMembers :: JSRef (T1 :|: T2) -> Object (Members T1)
testUnionMembers = toObject
