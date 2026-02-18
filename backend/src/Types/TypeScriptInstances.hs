{-# OPTIONS_GHC -Wno-orphans #-}

module Types.TypeScriptInstances () where

import Data.Aeson.TypeScript.TH (TSDeclaration (..), TypeScript (..))
import Data.Time (UTCTime)
import Servant (NoContent (..))
import Servant.Multipart (FileData, Tmp)

-- | UTCTime encodes to JSON as an ISO8601 string, so mirror that for TypeScript.
instance TypeScript UTCTime where
  getTypeScriptType _ = "string"

-- | Treat multipart file uploads as browser File objects for generated TS.
instance TypeScript (FileData Tmp) where
  getTypeScriptType _ = "File"

-- | Map Ordering to the FE union alias defined manually in TS.
instance TypeScript Ordering where
  getTypeScriptType _ = "ApiInequalityOperator"
  getTypeScriptDeclarations _ =
    [ TSRawDeclaration
        "export type ApiInequalityOperator = \"GT\" | \"LT\" | \"EQ\";"
    ]

-- | Encode Servant's NoContent as void in TypeScript.
instance TypeScript NoContent where
  getTypeScriptType _ = "void"
