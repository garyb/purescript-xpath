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

module Data.XPath.AST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)

data Expr
  = Var QName
  | Literal Literal
  | FunctionCall FunctionName (Array Expr)
  | Negated Expr
  | BinaryOpExpr Expr Operator Expr
  | PredicatedExpr Expr Expr
  | SelectorExpr Selector

derive instance eqExpr :: Eq Expr
derive instance ordExpr :: Ord Expr
derive instance genericExpr :: Generic Expr _
instance showExpr :: Show Expr where show e = genericShow e

data Literal
  = String String
  | Number Number
  | Int Int
  | Boolean Boolean

derive instance eqLiteral :: Eq Literal
derive instance ordLiteral :: Ord Literal
derive instance genericLiteral :: Generic Literal _
instance showLiteral :: Show Literal where show e = genericShow e

data Operator
  = Multiply
  | Divide
  | Modulo
  | Plus
  | Minus
  | LessThan
  | GreaterThan
  | LessThanOrEquals
  | GreaterThanOrEquals
  | Equals
  | NotEquals
  | And
  | Or

derive instance eqOperator :: Eq Operator
derive instance ordOperator :: Ord Operator
derive instance genericOperator :: Generic Operator _
instance showOperator :: Show Operator where show = genericShow

data Selector
  = AbsSelector (Maybe Path)
	| AbsDescendantOrSelfSelector Path
  | RelSelector Path
	| ChildPathSelector Selector Path
	| DescendantOrSelfPathSelector Selector Path
  | UnionSelector Selector Selector

derive instance eqSelector :: Eq Selector
derive instance ordSelector :: Ord Selector
derive instance genericSelector :: Generic Selector _
instance showSelector :: Show Selector where show s = genericShow s

data Path
  = StepPath Step
	| ChildPath Path Step
	| DescendantOrSelfPath Path Step

derive instance eqPath :: Eq Path
derive instance ordPath :: Ord Path
derive instance genericPath :: Generic Path _
instance showPath :: Show Path where show p = genericShow p

data Step
  = Step (Maybe Axis) NodeTest
  | PredicatedStep Step Expr
  | ContextStep
  | ContextParentStep

derive instance eqStep :: Eq Step
derive instance ordStep :: Ord Step
derive instance genericStep :: Generic Step _
instance showStep :: Show Step where show s = genericShow s

data Axis
  = Ancestor
  | AncestorOrSelf
  | Attribute
  | AttributeAbbr
  | Child
  | Descendant
  | DescendantOrSelf
  | Following
  | FollowingSibling
  | Namespace
  | Parent
  | Preceding
  | PrecedingSibling
  | Self

derive instance eqAxis :: Eq Axis
derive instance ordAxis :: Ord Axis
derive instance genericAxis :: Generic Axis _
instance showAxis :: Show Axis where show = genericShow

data NodeTest
  = AnyNodeTest
  | NamespaceTest NCName
  | NameTest QName
	| NodeType NodeType

derive instance eqNodeTest :: Eq NodeTest
derive instance ordNodeTest :: Ord NodeTest
derive instance genericNodeTest :: Generic NodeTest _
instance showNodeTest :: Show NodeTest where show = genericShow

data NodeType
  = Comment
  | Text
  | ProcessingInstruction String
  | Node

derive instance eqNodeType :: Eq NodeType
derive instance ordNodeType :: Ord NodeType
derive instance genericNodeType :: Generic NodeType _
instance showNodeType :: Show NodeType where show = genericShow

newtype FunctionName = FunctionName String

derive instance eqFunctionName :: Eq FunctionName
derive instance ordFunctionName :: Ord FunctionName
derive instance genericFunctionName :: Generic FunctionName _
instance showFunctionName :: Show FunctionName where show = genericShow

data QName = QName (Maybe NCName) NCName

derive instance eqQName :: Eq QName
derive instance ordQName :: Ord QName
derive instance genericQName :: Generic QName _
instance showQName :: Show QName where show = genericShow

newtype NCName = NCName String

derive instance eqNCName :: Eq NCName
derive instance ordNCName :: Ord NCName
derive instance genericNCName :: Generic NCName _
instance showNCName :: Show NCName where show = genericShow
