{-# LANGUAGE OverloadedStrings #-}
module Ringo.Generator.Sql where

import qualified Data.Text      as Text
import qualified Data.Text.Lazy as TL

import Database.HsSqlPpp.Annotation
import Database.HsSqlPpp.Dialect
import Database.HsSqlPpp.Pretty
import Database.HsSqlPpp.Syntax
import Data.Text (Text)

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
member = BinaryOp ea (name ".")

num :: Text -> ScalarExpr
num = NumberLit ea . Text.unpack

str :: Text -> ScalarExpr
str = StringLit ea . Text.unpack

extEpoch :: ScalarExpr -> ScalarExpr
extEpoch = Extract ea ExtractEpoch

app :: Text -> [ScalarExpr] -> ScalarExpr
app n = App ea (name n)

cast :: ScalarExpr -> Text -> ScalarExpr
cast ex = Cast ea ex . SimpleTypeName ea . name

prefop :: Text -> ScalarExpr -> ScalarExpr
prefop n = PrefixOp ea (name n)

postop :: Text -> ScalarExpr -> ScalarExpr
postop n = PostfixOp ea (name n)

binop :: Text -> ScalarExpr -> ScalarExpr -> ScalarExpr
binop n = BinaryOp ea (name n)

foldBinop :: Text -> [ScalarExpr] -> ScalarExpr
foldBinop _ [] = error "List must be non empty"
foldBinop n (a : as) = foldl (binop n) a as

placeholder :: ScalarExpr
placeholder = Placeholder ea

parens :: ScalarExpr -> ScalarExpr
parens = Parens ea

qstar :: Text -> ScalarExpr
qstar = QStar ea . nmc

star :: ScalarExpr
star = Star ea

subQueryExp :: QueryExpr -> ScalarExpr
subQueryExp = ScalarSubQuery ea

-- Table ref
tref :: Text -> TableRef
tref = Tref ea . name

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
sia = SelectItem ea

-- Expression qualified identifier
eqi :: Text -> Text -> ScalarExpr
eqi c = Identifier ea . qn c

-- Expression identifier
ei :: Text -> ScalarExpr
ei = Identifier ea . name

-- Qualified name
qn :: Text -> Text -> Name
qn c n = Name ea [nmc c, nmc n]

-- Select list
sl :: [SelectItem] -> SelectList
sl = SelectList ea

-- Insert statement
insert :: Text -> [Text] -> QueryExpr -> Statement
insert tName cNames selectExp =
  Insert ea (name tName) (map nmc cNames) selectExp Nothing

-- Update statement
update :: Text -> [(Text, ScalarExpr)] -> [TableRef] -> ScalarExpr -> Statement
update tName setClauseList fromList whr =
  Update ea (name tName) (map (uncurry (SetClause ea . nmc)) setClauseList) fromList (Just whr) Nothing

-- Pretty print statement
ppStatement :: Statement -> Text
ppStatement st = TL.toStrict $ prettyStatements (PrettyFlags postgresDialect) [st]

-- Pretty print scalar expression
ppScalarExpr :: ScalarExpr -> Text
ppScalarExpr = TL.toStrict . prettyScalarExpr (PrettyFlags postgresDialect)
