{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Coerce
import Data.HVect
import GHCJS.TypeScript.Function
import GHCJS.TypeScript.Object
import GHCJS.TypeScript.Types
import GHCJS.Types
import GHCJS.Prim
import Prelude hiding (String)
import Raw

foreign import javascript "window.Highcharts" highcharts :: HighchartsStatic

foreign import javascript "console.log($1)" output :: JSRef obj -> IO ()

main = do
  let xs = coerce (map toJSInt [1,1,2,3,5,8,13]) :: [Number]
  series <- join $ objectInsert (P::P"data") <$> listToArray xs <*> newObject
  chart <- newObject >>=
    objectInsert (P::P"renderTo") (toString "container") >>=
    objectInsert (P::P"type") (toString "column")
  title <- newObject >>=
    objectInsert (P::P"text") (toString "Test graph")
  subtitle <- newObject >>=
    objectInsert (P::P"text") (toString "Test subtitle")
  arr <- listToArray ([] :: [String])
  xAxis <- newObject >>=
    objectInsert (P::P"categories") arr
  title2 <- newObject >>=
    objectInsert (P::P"text") (toString "Fibs")
  yAxis <- newObject >>=
    objectInsert (P::P"title") title2
  seriess <- listToArray [series]
  options <- newObject >>=
    objectInsert (P::P"chart") chart >>=
    objectInsert (P::P"title") title >>=
    objectInsert (P::P"subtitle") subtitle >>=
    objectInsert (P::P"xAxis") xAxis >>=
    objectInsert (P::P"yAxis") yAxis >>=
    objectInsert (P::P"series") seriess
  constr <- getProperty (P::P"Chart") highcharts
  let castedOptions = cast options :: HighchartsOptions
  result <- invokeConstructor constr (castedOptions :&: HNil)
  -- FIXME: make this work
  -- result <- construct (P::P"Chart") (options :&: HNil) highcharts
  output (coerce result)
