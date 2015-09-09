module GHCJS.TypeScript.Convert where

import GHCJS.TypeScript.Convert.Collect (collect)
import GHCJS.TypeScript.Convert.Munge
import GHCJS.TypeScript.Convert.Render (render)
import GHCJS.TypeScript.Convert.Types
import GHCJS.TypeScript.Convert.Util
import Language.TypeScript
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Text.Parsec (parse)

convertTypeScript :: Config -> FilePath -> IO [ModuleName]
convertTypeScript config fp = do
  parsed <- parseTypeScript fp
  let outputs = render (collect parsed)
  mapM_ (writeOutput config) outputs
  return (map fst outputs)

parseTypeScript :: FilePath -> IO [DeclarationElement]
parseTypeScript fp =
  either (fail . show) return .
  parse declarationSourceFile fp =<<
  readFile fp

writeOutput :: Config -> (ModuleName, OutputModule) -> IO ()
writeOutput config (mn, OutputModule imports contents) = do
   let fp = outputDir config </> moduleNamePath mn
   createDirectoryIfMissing True (takeDirectory fp)
   writeFile fp $ unlines $
     pragmas ++
     ["module " ++ renderModuleName mn ++ " where"] ++
     imports ++
     contents

pragmas :: [String]
pragmas =
  [ "{-# LANGUAGE ConstraintKinds            #-}"
  , "{-# LANGUAGE DataKinds                  #-}"
  , "{-# LANGUAGE FlexibleContexts           #-}"
  , "{-# LANGUAGE FlexibleInstances          #-}"
  , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
  , "{-# LANGUAGE NoImplicitPrelude          #-}"
  , "{-# LANGUAGE PolyKinds                  #-}"
  , "{-# LANGUAGE RankNTypes                 #-}"
  , "{-# LANGUAGE TypeFamilies               #-}"
  , "{-# LANGUAGE TypeOperators              #-}"
  , "{-# LANGUAGE UndecidableInstances       #-}"
  ]

defaultConfig :: FilePath -> Config
defaultConfig out = Config
  { outputDir = out
  }
