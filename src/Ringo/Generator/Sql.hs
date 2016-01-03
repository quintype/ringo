{-# LANGUAGE OverloadedStrings #-}
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

attDef :: Text -> Text -> RowConstraint -> AttributeDef
attDef nam typ constr =
  AttributeDef ea (nmc nam) (SimpleTypeName ea $ name typ) Nothing [constr]

member :: ScalarExpr -> ScalarExpr -> ScalarExpr
member a b = BinaryOp ea (name ".") a b

num :: Text -> ScalarExpr
num n = NumberLit ea $ Text.unpack n

str :: Text -> ScalarExpr
str = StringLit ea . Text.unpack

app :: Text -> [ScalarExpr] -> ScalarExpr
app n as = App ea (name n) as

specop :: Text -> [ScalarExpr] -> ScalarExpr
specop n as = SpecialOp ea (name n) as

prefop :: Text -> ScalarExpr -> ScalarExpr
prefop n a = PrefixOp ea (name n) a

postop :: Text -> ScalarExpr -> ScalarExpr
postop n a = PostfixOp ea (name n) a

binop :: Text -> ScalarExpr -> ScalarExpr -> ScalarExpr
binop n a0 a1 = BinaryOp ea (name n) a0 a1

foldBinop :: Text -> [ScalarExpr] -> ScalarExpr
foldBinop _ [] = error "List must be non empty"
foldBinop n (a : as) = foldl (binop n) a as

placeholder :: ScalarExpr
placeholder = Placeholder ea

parens :: ScalarExpr -> ScalarExpr
parens = Parens ea

qstar :: Text -> ScalarExpr
qstar = QStar ea . nmc

-- Table ref
tref :: Text -> TableRef
tref s = Tref ea (name s)

-- Table ref alias
trefa :: Text -> Text -> TableRef
trefa t a = TableAlias ea (nmc a) $ Tref ea (name t)

-- Subquery Table ref alias
subtrefa :: Text -> QueryExpr -> TableRef
subtrefa a = TableAlias ea (nmc a) . SubTref ea

-- Table join
tjoin :: TableRef -> JoinType -> TableRef -> Maybe ScalarExpr -> TableRef
tjoin ta jt tb on = JoinTref ea ta Unnatural jt Nothing tb (fmap (JoinOn ea) on)

-- Select item
si :: ScalarExpr -> SelectItem
si = SelExp ea

-- Select item alias
sia :: ScalarExpr -> NameComponent -> SelectItem
sia e a = SelectItem ea e a

-- Expression qualified identifier
eqi :: Text -> Text -> ScalarExpr
eqi c x = Identifier ea $ qn c x

-- Expression identifier
ei :: Text -> ScalarExpr
ei = Identifier ea . name

-- Qualified name
qn :: Text -> Text -> Name
qn c n = Name ea [nmc c, nmc n]

-- Select list
sl :: [SelectItem] -> SelectList
sl = SelectList ea

insert :: Text -> [Text] -> QueryExpr -> Statement
insert tName cNames selectExp =
  Insert ea (name tName) (map nmc cNames) selectExp Nothing

ppSQL :: Statement -> Text
ppSQL st = TL.toStrict $ prettyStatements (PrettyFlags postgresDialect) [st]

ppScalarExpr :: ScalarExpr -> Text
ppScalarExpr = TL.toStrict . prettyScalarExpr (PrettyFlags postgresDialect)

ppQueryExpr :: QueryExpr -> Text
ppQueryExpr = TL.toStrict . prettyQueryExpr (PrettyFlags postgresDialect)
