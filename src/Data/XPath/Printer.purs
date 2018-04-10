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

module Data.XPath.Printer where

import Data.XPath.AST

import Data.Maybe (maybe)
import Data.String as String
import Prelude (map, show, ($), (<>), (>))

printPath ∷ Path → String
printPath = case _ of
  StepPath step → printStep step
  ChildPath path step → printPath path <> "/" <> printStep step
  DescendantOrSelfPath path step → printPath path <> "//" <> printStep step

printStep ∷ Step → String
printStep = case _ of
  Step maxis nodeTest → maybe "" printAxis maxis <> printNodeTest nodeTest
  PredicatedStep s pred → printStep s <> printPredicate pred
  ContextStep → "."
  ContextParentStep → ".."

printAxis ∷ Axis → String
printAxis = case _ of
  Ancestor → "ancestor::"
  AncestorOrSelf → "ancestor-or-self::"
  Attribute → "attribute::"
  AttributeAbbr → "@"
  Child → "child::"
  Descendant → "descendant::"
  DescendantOrSelf → "descendant-or-self::"
  Following → "following::"
  FollowingSibling → "following-sibling::"
  Namespace → "namespace::"
  Parent → "parent::"
  Preceding → "preceding::"
  PrecedingSibling → "preceding-sibling::"
  Self → "self::"

printNodeTest ∷ NodeTest → String
printNodeTest = case _ of
  AnyNodeTest → "*"
  NamespaceTest name → printNCName name <> ":*"
  NameTest name → printQName name
  NodeType (ProcessingInstruction s) → "processing-instruction(" <> printStringLiteral s <> ")"
  NodeType nt → printNodeType nt <> "()"

printNodeType ∷ NodeType → String
printNodeType = case _ of
  Comment → "comment"
  Text → "text"
  ProcessingInstruction _ → "processing-instruction"
  Node → "node"

printPredicate ∷ Expr → String
printPredicate expr = "[" <> printExpr expr <> "]"

printExpr ∷ Expr → String
printExpr e = printExprPrec 0 e

printExprPrec ∷ Int → Expr → String
printExprPrec p e =
   case e of
    Var name →
      "$" <> printQName name
    Literal lit →
      printLiteral lit
    FunctionCall fn args →
      printFunctionName fn
        <> "("
        <> String.joinWith ", " (map printExpr args)
        <> ")"
    Negated expr →
      withParens (p > 7) ("-" <> printExprPrec 7 expr)
    BinaryOpExpr lhs op rhs →
      withParens (p > precedence op) $
        printExprPrec (precedence op) lhs
          <> " " <> printOperator op <> " "
          <> printExprPrec (precedence op) rhs
    SelectorExpr sel →
      printSelector sel
    PredicatedExpr expr pred →
      printExprPrec 2 expr <> printPredicate pred

printLiteral :: Literal -> String
printLiteral = case _ of
  String s → printStringLiteral s
  Number n → show n
  Int n → show n
  Boolean true -> "true()"
  Boolean false -> "false()"

printStringLiteral ∷ String → String
printStringLiteral = show -- TODO: proper xpath-compliant printing

printSelector ∷ Selector → String
printSelector s = printSelectorPrec 0 s

printSelectorPrec ∷ Int → Selector → String
printSelectorPrec p = case _ of
  AbsSelector mpath →
    "/" <> maybe "" printPath mpath
  AbsDescendantOrSelfSelector path →
    "//" <> printPath path
  RelSelector path →
    printPath path
  ChildPathSelector sel path →
    printSelectorPrec 2 sel <> "/" <> printPath path
  DescendantOrSelfPathSelector sel path →
    printSelectorPrec 2 sel <> "//" <> printPath path
  UnionSelector lhs rhs →
    withParens (p > 1) (printSelector lhs <> " | " <> printSelector rhs)

withParens ∷ Boolean → String → String
withParens b s = if b then "(" <> s <> ")" else s

printOperator ∷ Operator → String
printOperator = case _ of
  Multiply → "*"
  Divide → "div"
  Modulo → "mod"
  Plus → "+"
  Minus → "-"
  LessThan → "<"
  GreaterThan → ">"
  LessThanOrEquals → "<="
  GreaterThanOrEquals → ">="
  Equals → "="
  NotEquals → "!="
  And → "and"
  Or → "or"

precedence ∷ Operator → Int
precedence = case _ of
  Multiply → 5
  Divide → 5
  Modulo → 5
  Plus → 4
  Minus → 4
  LessThan → 3
  GreaterThan → 3
  LessThanOrEquals → 3
  GreaterThanOrEquals → 3
  Equals → 2
  NotEquals → 2
  And → 1
  Or → 0

printFunctionName ∷ FunctionName → String
printFunctionName (FunctionName name) = name

printQName ∷ QName → String
printQName (QName prefix local) =
  maybe "" (\p -> printNCName p <> ":") prefix <> printNCName local

printNCName ∷ NCName → String
printNCName (NCName name) = name
