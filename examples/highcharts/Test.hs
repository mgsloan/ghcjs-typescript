{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Coerce
import Data.HVect
import GHCJS.TypeScript
import GHCJS.Types
import GHCJS.Prim
import Prelude hiding (String)
import Raw

foreign import javascript "window.Highcharts" highcharts :: HighchartsStatic

foreign import javascript "console.log($1)" output :: JSRef obj -> IO ()

main = do
  let xs = coerce (map toJSInt [1,1,2,3,5,8,13]) :: [Number]
  series <- join $ objAdd (P::P"data") <$> listToArray xs <*> newObj
  chart <- newObj >>=
    objAdd (P::P"renderTo") (toString "container") >>=
    objAdd (P::P"type") (toString "column")
  title <- newObj >>=
    objAdd (P::P"text") (toString "Test graph")
  subtitle <- newObj >>=
    objAdd (P::P"text") (toString "Test subtitle")
  arr <- listToArray ([] :: [String])
  xAxis <- newObj >>=
    objAdd (P::P"categories") arr
  title2 <- newObj >>=
    objAdd (P::P"text") (toString "Fibs")
  yAxis <- newObj >>=
    objAdd (P::P"title") title2
  seriess <- listToArray [series]
  options <- newObj >>=
    objAdd (P::P"chart") chart >>=
    objAdd (P::P"title") title >>=
    objAdd (P::P"subtitle") subtitle >>=
    objAdd (P::P"xAxis") xAxis >>=
    objAdd (P::P"yAxis") yAxis >>=
    objAdd (P::P"series") seriess
  result <- callConstructor (P::P"Chart") (options :&: HNil) highcharts
  output (coerce result)
