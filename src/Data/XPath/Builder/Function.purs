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

module Data.XPath.Builder.Function
  ( module Data.XPath.Function
  , module Data.XPath.Builder.Function
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.XPath.AST (Expr)
import Data.XPath.Builder (class IsExpr, class IsSelector, toExpr, toSelector)
import Data.XPath.Function (last, position)
import Data.XPath.Function as XF

-- | The `count(node-set)` function.
count ∷ ∀ s. IsSelector s ⇒ s → Expr
count = XF.count <<< toSelector

-- | The `id(any)` function.
id ∷ ∀ e. IsExpr e ⇒ e → Expr
id = XF.id <<< toExpr

-- | The `local-name(node-set?)` function.
localName ∷ ∀ s. IsSelector s ⇒ Maybe s → Expr
localName = XF.localName <<< map toSelector

-- | The `namespace-uri(node-set?)` function.
namespaceURI ∷ ∀ s. IsSelector s ⇒ Maybe s → Expr
namespaceURI = XF.namespaceURI <<< map toSelector

-- | The `name(node-set?)` function.
name ∷ ∀ s. IsSelector s ⇒ Maybe s → Expr
name = XF.name <<< map toSelector



string ∷ ∀ e. IsExpr e ⇒ Maybe e → Expr
string = XF.string <<< map toExpr

concat ∷ ∀ e1 e2 e3. IsExpr e1 ⇒ IsExpr e2 ⇒ IsExpr e3 ⇒ e1 → e2 → Array e3 → Expr
concat x y = XF.concat (toExpr x) (toExpr y) <<< map toExpr

startsWith ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
startsWith x = XF.startsWith (toExpr x) <<< toExpr

contains ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
contains x = XF.contains (toExpr x) <<< toExpr

substringBefore ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
substringBefore x = XF.substringBefore (toExpr x) <<< toExpr

substringAfter ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
substringAfter x = XF.substringAfter (toExpr x) <<< toExpr

substring ∷ ∀ e1 e2 e3. IsExpr e1 ⇒ IsExpr e2 ⇒ IsExpr e3 ⇒ e1 → e2 → Maybe e3 → Expr
substring x y = XF.substring (toExpr x) (toExpr y) <<< map toExpr

stringLength ∷ ∀ e. IsExpr e ⇒ Maybe e → Expr
stringLength = XF.stringLength <<< map toExpr

normalizeSpace ∷ ∀ e. IsExpr e ⇒ Maybe e → Expr
normalizeSpace = XF.normalizeSpace <<< map toExpr

translate ∷ ∀ e1 e2 e3. IsExpr e1 ⇒ IsExpr e2 ⇒ IsExpr e3 ⇒ e1 → e2 → e3 → Expr
translate x y = XF.translate (toExpr x) (toExpr y) <<< toExpr



boolean ∷ ∀ e. IsExpr e ⇒ Maybe e → Expr
boolean = XF.boolean <<< map toExpr

not ∷ ∀ e. IsExpr e ⇒ e → Expr
not = XF.not <<< toExpr

lang ∷ ∀ e. IsExpr e ⇒ e → Expr
lang = XF.lang <<< toExpr



number ∷ ∀ e. IsExpr e ⇒ Maybe e → Expr
number = XF.number <<< map toExpr

sum ∷ ∀ e. IsExpr e ⇒ e → Expr
sum = XF.sum <<< toExpr

floor ∷ ∀ e. IsExpr e ⇒ e → Expr
floor = XF.floor <<< toExpr

ceil ∷ ∀ e. IsExpr e ⇒ e → Expr
ceil = XF.ceil <<< toExpr

round ∷ ∀ e. IsExpr e ⇒ e → Expr
round = XF.round <<< toExpr
