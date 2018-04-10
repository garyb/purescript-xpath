{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Data.XPath.Function where

import Prelude

import Data.Maybe (Maybe, maybe)
import Data.XPath.AST (Expr(..), FunctionName(..), Selector)

-- | The `last()` function.
last :: Expr
last = FunctionCall (FunctionName "last") []

-- | The `position()` function.
position :: Expr
position = FunctionCall (FunctionName "position") []

-- | The `count(node-set)` function.
count :: Selector -> Expr
count s = FunctionCall (FunctionName "count") [SelectorExpr s]

-- | The `id(any)` function.
id :: Expr -> Expr
id e = FunctionCall (FunctionName "id") [e]

-- | The `local-name(node-set?)` function.
localName :: Maybe Selector -> Expr
localName ms = FunctionCall (FunctionName "local-name") (maybe [] (pure <<< SelectorExpr) ms)

-- | The `namespace-uri(node-set?)` function.
namespaceURI :: Maybe Selector -> Expr
namespaceURI ms = FunctionCall (FunctionName "namespace-uri") (maybe [] (pure <<< SelectorExpr) ms)

-- | The `name(node-set?)` function.
name :: Maybe Selector -> Expr
name ms = FunctionCall (FunctionName "name") (maybe [] (pure <<< SelectorExpr) ms)



string :: Maybe Expr -> Expr
string mx = FunctionCall (FunctionName "string") (maybe [] pure mx)

concat :: Expr -> Expr -> Array Expr -> Expr
concat x y zs = FunctionCall (FunctionName "concat") ([x, y] <> zs)

startsWith :: Expr -> Expr -> Expr
startsWith x y = FunctionCall (FunctionName "starts-with") [x, y]

contains :: Expr -> Expr -> Expr
contains x y = FunctionCall (FunctionName "contains") [x, y]

substringBefore :: Expr -> Expr -> Expr
substringBefore x y = FunctionCall (FunctionName "substring-before") [x, y]

substringAfter :: Expr -> Expr -> Expr
substringAfter x y = FunctionCall (FunctionName "substring-after") [x, y]

substring :: Expr -> Expr -> Maybe Expr -> Expr
substring x y mz = FunctionCall (FunctionName "substring") ([x, y] <> maybe [] pure mz)

stringLength :: Maybe Expr -> Expr
stringLength mx = FunctionCall (FunctionName "string-length") (maybe [] pure mx)

normalizeSpace :: Maybe Expr -> Expr
normalizeSpace mx = FunctionCall (FunctionName "normalize-space") (maybe [] pure mx)

translate :: Expr -> Expr -> Expr -> Expr
translate x y z = FunctionCall (FunctionName "translate") [x, y, z]



boolean :: Maybe Expr -> Expr
boolean mx = FunctionCall (FunctionName "boolean") (maybe [] pure mx)

not :: Expr -> Expr
not x = FunctionCall (FunctionName "not") [x]

lang :: Expr -> Expr
lang x = FunctionCall (FunctionName "lang") [x]



number :: Maybe Expr -> Expr
number mx = FunctionCall (FunctionName "number") (maybe [] pure mx)

sum :: Expr -> Expr
sum x = FunctionCall (FunctionName "sum") [x]

floor :: Expr -> Expr
floor x = FunctionCall (FunctionName "floor") [x]

ceil :: Expr -> Expr
ceil x = FunctionCall (FunctionName "ceil") [x]

round :: Expr -> Expr
round x = FunctionCall (FunctionName "round") [x]
