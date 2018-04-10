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

module Data.XPath.Builder
  ( module Data.XPath.Builder
  , module Exports
  ) where

import Prelude ((<<<))

import Data.Maybe (Maybe(..))
import Data.XPath.AST
import Data.XPath.AST (Expr, Selector, Path, Axis(..), NodeType(..)) as Exports

-- | A class for types that can be embedded in an XPath `Expr`.
class IsExpr t where
  toExpr ∷ t → Expr

instance isExprExpr ∷ IsExpr Expr where
  toExpr x = x

instance isExprString ∷ IsExpr String where
  toExpr = Literal <<< String

instance isExprNumber ∷ IsExpr Number where
  toExpr = Literal <<< Number

instance isExprInt ∷ IsExpr Int where
  toExpr = Literal <<< Int

instance isExprSelector ∷ IsExpr Selector where
  toExpr = SelectorExpr

-- | Paths injected into `Expr` are assumed to be relative. Explicit use of
-- | `abs` is required to make an absolute path.
instance isExprPath ∷ IsExpr Path where
  toExpr = SelectorExpr <<< RelSelector

-- | Path steps injected into `Expr` are assumed to be treated as relative
-- | paths. Explicit use of `abs` is required to make an absolute path.
instance isExprStep ∷ IsExpr Step where
  toExpr = SelectorExpr <<< RelSelector <<< StepPath

-- | A class for types that can be embedded in an XPath `Selector`.
class IsSelector t where
  toSelector ∷ t → Selector

instance isSelectorSelector ∷ IsSelector Selector where
  toSelector x = x

-- | Paths lifted into `Selector` are assumed to be relative. Explicit use of
-- | `abs` is required to make an absolute path.
instance isSelectorPath ∷ IsSelector Path where
  toSelector = RelSelector

-- | Path steps injected into `Selector` are assumed to be treated as relative
-- | paths. Explicit use of `abs` is required to make an absolute path.
instance isSelectorStep ∷ IsSelector Step where
  toSelector = RelSelector <<< StepPath

-- | A class for types that can be embedded in an XPath `Path`.
class IsPath t where
  toPath ∷ t → Path

instance isPathPath ∷ IsPath Path where
  toPath x = x

instance isPathStep ∷ IsPath Step where
  toPath = StepPath

-- | A class for types that can have an XPath predicate expression attached.
class Predicated t u | t -> u where
  withPredicate ∷ ∀ e. IsExpr e ⇒ t → e → u

instance predicatedExpr ∷ Predicated Expr Expr where
  withPredicate e = PredicatedExpr e <<< toExpr

instance predicatedSelector ∷ Predicated Selector Expr where
  withPredicate s = PredicatedExpr (SelectorExpr s) <<< toExpr

instance predicatedStep ∷ Predicated Step Step where
  withPredicate s = PredicatedStep s <<< toExpr

mult ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
mult x y = BinaryOpExpr (toExpr x) Multiply (toExpr y)

div ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
div x y = BinaryOpExpr (toExpr x) Divide (toExpr y)

mod ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
mod x y = BinaryOpExpr (toExpr x) Modulo (toExpr y)

plus ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
plus x y = BinaryOpExpr (toExpr x) Plus (toExpr y)

minus ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
minus x y = BinaryOpExpr (toExpr x) Minus (toExpr y)

lessThan ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
lessThan x y = BinaryOpExpr (toExpr x) LessThan (toExpr y)

greaterThan ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
greaterThan x y = BinaryOpExpr (toExpr x) GreaterThan (toExpr y)

lessThanOrEquals ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
lessThanOrEquals x y = BinaryOpExpr (toExpr x) LessThanOrEquals (toExpr y)

greaterThanOrEquals ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
greaterThanOrEquals x y = BinaryOpExpr (toExpr x) GreaterThanOrEquals (toExpr y)

equals ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
equals x y = BinaryOpExpr (toExpr x) Equals (toExpr y)

notEquals ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
notEquals x y = BinaryOpExpr (toExpr x) NotEquals (toExpr y)

and ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
and x y = BinaryOpExpr (toExpr x) And (toExpr y)

or ∷ ∀ e1 e2. IsExpr e1 ⇒ IsExpr e2 ⇒ e1 → e2 → Expr
or x y = BinaryOpExpr (toExpr x) Or (toExpr y)

infixl 11 mult as *.
infixl 11 div as /.
infixl 11 mod as %.
infixl 10 plus as +.
infixl 10 minus as -.
infixl 9 lessThan as <.
infixl 9 greaterThan as >.
infixl 9 lessThanOrEquals as <=.
infixl 9 greaterThanOrEquals as >=.
infixl 8 equals as ==.
infixl 8 notEquals as /=.
infixl 7 and as &&.
infixl 6 or as ||.

-- | Modified the `Axis` part of a `Step`. This will have no effect if
-- | applied to the abbreviated `/` or `//` steps.
withAxis ∷ Axis → Step → Step
withAxis ax = case _ of
  Step _ nt → Step (Just ax) nt
  s → s

infixl 12 withAxis as <:>

-- | A class for path-like or path-containing types that allow the path to be
-- | further extended.
-- |
-- | ``` purescript
-- | childPath (named "section") (named "heading") ~= section/heading
-- | childPath any (named "p") ~= */bar
-- |
-- | descendantOrSelfPath (named "section") (named "heading") ~= section//heading
-- | descendantOrSelfPath any (named "p") ~= *//bar
-- | ```
class PathExtendable t a r | r → t a where
  childPath ∷ t → a → r
  descendantOrSelfPath ∷ t → a → r

instance pathExtendablePath ∷ IsPath p ⇒ PathExtendable p Step Path where
  childPath = ChildPath <<< toPath
  descendantOrSelfPath = DescendantOrSelfPath <<< toPath

instance pathExtendableSelector ∷ (IsSelector s, IsPath p) ⇒ PathExtendable s p Selector where
  childPath s p = ChildPathSelector (toSelector s) (toPath p)
  descendantOrSelfPath s p = DescendantOrSelfPathSelector (toSelector s) (toPath p)

infixl 5 childPath as </>
infixl 5 descendantOrSelfPath as <//>

-- | A selector from the union of two other selectors.
union ∷ ∀ s1 s2. IsSelector s1 ⇒ IsSelector s2 ⇒ s1 → s2 → Selector
union x y = UnionSelector (toSelector x) (toSelector y)

infixl 4 union as |.

-- | A path step that selects node(s) with a particular name.
named ∷ String → Step
named name = Step Nothing (NameTest (QName Nothing (NCName name)))

-- | A path step that selects node(s) with a particular namespace-qualified
-- | name.
qnamed ∷ String → String → Step
qnamed qname lname =
  Step Nothing (NameTest (QName (Just (NCName qname)) (NCName lname)))

-- | A path step that selects attribute node(s) with a particular name, using
-- | the abbreviated attribute selector (`@`).
attr ∷ String → Step
attr name = Step (Just AttributeAbbr) (NameTest (QName Nothing (NCName name)))

-- | A path step that selects attribute node(s) with a particular
-- | namespace-qualified name, using the abbreviated attribute selector (`@`).
qattr ∷ String → String → Step
qattr qname lname =
  Step (Just AttributeAbbr) (NameTest (QName (Just (NCName qname)) (NCName lname)))

-- | A path step that selects node(s) os the specified type.
nodeType ∷ NodeType → Step
nodeType ty = Step Nothing (NodeType ty)

comment ∷ Step
comment = nodeType Comment

text ∷ Step
text = nodeType Text

processingInstruction ∷ String → Step
processingInstruction = nodeType <<< ProcessingInstruction

node ∷ Step
node = nodeType Node

-- | A path step that will select any node(s).
any ∷ Step
any = Step Nothing AnyNodeTest

-- | A path step for the context node (`.`).
context ∷ Step
context = ContextStep

-- | A path step for the context node's parent (`..`).
contextParent ∷ Step
contextParent = ContextParentStep

-- | A relative path selector.
rel ∷ ∀ p. IsPath p ⇒ p → Selector
rel = RelSelector <<< toPath

-- | The root node selector using the abbreviated syntax (`/`).
root ∷ Selector
root = AbsSelector Nothing

-- | An absolute path selector using the abbreviated syntax (`/...`).
abs ∷ ∀ p. IsPath p ⇒ p → Selector
abs = AbsSelector <<< Just <<< toPath

-- | An absolute descendant-or-self path selector using the abbreviated syntax
-- | (`//...`).
absDescendantOrSelf ∷ ∀ p. IsPath p ⇒ p → Selector
absDescendantOrSelf = AbsDescendantOrSelfSelector <<< toPath
