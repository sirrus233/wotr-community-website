{-# OPTIONS_GHC -Wno-orphans #-}

module Types.TypeScriptInstances () where

import Data.Aeson.TypeScript.TH (TSDeclaration (..), TypeScript (..))
import Data.Time (UTCTime)
import Servant (NoContent (..))
import Servant.Multipart (FileData, Tmp)

instance TypeScript UTCTime where
  getTypeScriptType _ = "string"

instance TypeScript (FileData Tmp) where
  getTypeScriptType _ = "File"

instance TypeScript Ordering where
  getTypeScriptType _ = "ApiInequalityOperator"
  getTypeScriptDeclarations _ =
    [ TSRawDeclaration
        "type ApiInequalityOperator = \"GT\" | \"LT\" | \"EQ\";"
    ]

instance TypeScript NoContent where
  getTypeScriptType _ = "void"
