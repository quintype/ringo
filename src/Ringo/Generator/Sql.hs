module Ringo.Generator.Sql where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL


import Database.HsSqlPpp.Annotation
import Database.HsSqlPpp.Dialect (postgresDialect)
import Database.HsSqlPpp.Pretty
import Database.HsSqlPpp.Syntax
import Data.Text                 (Text)

ea :: Annotation
ea = emptyAnnotation

name :: Text -> Name
name n = Name ea [nmc n]

nmc :: Text -> NameComponent
nmc = Nmc . Text.unpack

att :: Text -> Text -> RowConstraint -> AttributeDef
att nam typ constr =
  AttributeDef ea (nmc nam) (SimpleTypeName ea $ name typ) Nothing [constr]

ppSQL :: Statement -> Text
ppSQL st = TL.toStrict $ prettyStatements (PrettyFlags postgresDialect) [st]
