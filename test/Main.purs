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

module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.XPath.AST as X
import Data.XPath.Builder ((&&.), (*.), (+.), (-.), (<//>), (</>), (<:>), (==.), (>.), (|.), (||.))
import Data.XPath.Builder as XB
import Data.XPath.Function as XF
import Data.XPath.Printer as XP
import Mote (Mote, group, test)
import Mote.Plan (Plan(..), PlanItem(..))
import MoteRunner (moteCli)
import Node.Process (PROCESS)
import Test.Assert (ASSERT, assertEqual)

type Test eff = Mote (Const Void) (Eff (assert :: ASSERT, console :: CONSOLE | eff) Unit) Unit

suite :: forall eff. Test eff
suite = do
  group "Expression precedence printing" do
    printExprTest
      "1 + 2 * 3"
      (X.BinaryOpExpr (X.Literal (X.Int 1)) X.Plus (X.BinaryOpExpr (X.Literal (X.Int 2)) X.Multiply (X.Literal (X.Int 3))))
    printExprTest
      "(1 + 2) * 3"
      (X.BinaryOpExpr (X.BinaryOpExpr (X.Literal (X.Int 1)) X.Plus (X.Literal (X.Int 2))) X.Multiply (X.Literal (X.Int 3)))

  group "Expression building operator precedence" do
    printExprTest
      "1 + 2 * 3"
      (1 +. 2 *. 3)
    printExprTest
      "(1 + 2) * 3"
      ((1 +. 2) *. 3)

  group "Selector printing examples" do
    printExprTest
      "child::para"
      (X.Child <:> XB.named "para")
    printExprTest
      "child::*"
      (X.Child <:> XB.any)
    printExprTest
      "child::text()"
      (X.Child <:> XB.nodeType X.Text)
    printExprTest
      "child::node()"
      (X.Child <:> XB.nodeType X.Node)
    printExprTest
      "attribute::name"
      (X.Attribute <:> XB.named "name")
    printExprTest
      "attribute::*"
      (X.Attribute <:> XB.any)
    printExprTest
      "descendant::para"
      (X.Descendant <:> XB.named "para")
    printExprTest
      "ancestor::div"
      (X.Ancestor <:> XB.named "div")
    printExprTest
      "ancestor-or-self::div"
      (X.AncestorOrSelf <:> XB.named "div")
    printExprTest
      "descendant-or-self::para"
      (X.DescendantOrSelf <:> XB.named "para")
    printExprTest
      "self::para"
      (X.Self <:> XB.named "para")
    printExprTest
      "child::chapter/descendant::para"
      (X.Child <:> XB.named "chapter" </> X.Descendant <:> XB.named "para")
    printExprTest
      "child::*/child::para"
      (X.Child <:> XB.any </> X.Child <:> XB.named "para")
    printExprTest
      "/"
      XB.root
    printExprTest
      "/descendant::para"
      (XB.abs (X.Descendant <:> XB.named "para"))
    printExprTest
      "/descendant::olist/child::item"
      (XB.abs
        (X.Descendant <:> XB.named "olist" </> X.Child <:> XB.named "item"))
    printExprTest
      "child::para[position() = 1]"
      (XB.withPredicate
        (X.Child <:> XB.named "para")
        (XF.position ==. 1))
    printExprTest
      "child::para[position() = last()]"
      (XB.withPredicate
        (X.Child <:> XB.named "para")
        (XF.position ==. XF.last))
    printExprTest
      "child::para[position() = last() - 1]"
      (XB.withPredicate
        (X.Child <:> XB.named "para")
        (XF.position ==. XF.last -. 1))
    printExprTest
      "child::para[position() > 1]"
      (XB.withPredicate
        (X.Child <:> XB.named "para")
        (XF.position >. 1))
    printExprTest
      "following-sibling::chapter[position() = 1]"
      (XB.withPredicate
        (X.FollowingSibling <:> XB.named "chapter")
        (XF.position ==. 1))
    printExprTest
      "preceding-sibling::chapter[position() = 1]"
      (XB.withPredicate
        (X.PrecedingSibling <:> XB.named "chapter")
        (XF.position ==. 1))
    printExprTest
      "/descendant::figure[position() = 42]"
      (XB.abs
        (XB.withPredicate
          (X.Descendant <:> XB.named "figure")
          (XF.position ==. 42)))
    printExprTest
      "/child::doc/child::chapter[position() = 5]/child::section[position() = 2]"
      (XB.abs
        (X.Child <:> XB.named "doc"
          </> XB.withPredicate (X.Child <:> XB.named "chapter") (XF.position ==. 5)
          </> XB.withPredicate (X.Child <:> XB.named "section") (XF.position ==. 2)))
    printExprTest
      """child::para[attribute::type = "warning"]"""
      (XB.withPredicate
        (X.Child <:> XB.named "para")
        (X.Attribute <:> XB.named "type" ==. "warning"))
    printExprTest
      """child::para[attribute::type = "warning"][position() = 5]"""
      (XB.withPredicate
        (XB.withPredicate
          (X.Child <:> XB.named "para")
          (X.Attribute <:> XB.named "type" ==. "warning"))
        (XF.position ==. 5))
    printExprTest
      """child::para[position() = 5][attribute::type = "warning"]"""
      (XB.withPredicate
        (XB.withPredicate
          (X.Child <:> XB.named "para")
          (XF.position ==. 5))
        (X.Attribute <:> XB.named "type" ==. "warning"))
    printExprTest
      """child::chapter[child::title = "Introduction"]"""
      (XB.withPredicate
        (X.Child <:> XB.named "chapter")
        (X.Child <:> XB.named "title" ==. "Introduction"))
    printExprTest
      "child::chapter[child::title]"
      (XB.withPredicate
        (X.Child <:> XB.named "chapter")
        (X.Child <:> XB.named "title"))
    printExprTest
      "child::*[self::chapter or self::appendix]"
      (XB.withPredicate
        (X.Child <:> XB.any)
        (X.Self <:> XB.named "chapter" ||. X.Self <:> XB.named "appendix"))
    printExprTest
      "child::*[self::chapter or self::appendix][position() = last()]"
      (XB.withPredicate
        (XB.withPredicate
          (X.Child <:> XB.any)
          (X.Self <:> XB.named "chapter" ||. X.Self <:> XB.named "appendix"))
        (XF.position ==. XF.last))

    printExprTest
      "para"
      (XB.named "para")
    printExprTest
      "*"
      XB.any
    printExprTest
      "text()"
      (XB.nodeType X.Text)
    printExprTest
      "@name"
      (XB.attr "name")
    printExprTest
      "@*"
      (XB.withAxis X.AttributeAbbr XB.any)
    printExprTest
      "para[1]"
      (XB.withPredicate (XB.named "para") 1)
    printExprTest
      "para[last()]"
      (XB.withPredicate (XB.named "para") XF.last)
    printExprTest
      "*/para"
      (XB.any </> XB.named "para")
    printExprTest
      "/doc/chapter[5]/section[2]"
      (XB.abs
        (XB.named "doc"
          </> XB.withPredicate (XB.named "chapter") 5
          </> XB.withPredicate (XB.named "section") 2))
    printExprTest
      "chapter//para"
      (XB.named "chapter" <//> XB.named "para")
    printExprTest
      "//para"
      (XB.absDescendantOrSelf (XB.named "para"))
    printExprTest
      "//olist/item"
      (XB.absDescendantOrSelf (XB.named "olist" </> XB.named "item"))
    printExprTest
      "."
      ( XB.context)
    printExprTest
      ".//para"
      (XB.context <//> XB.named "para")
    printExprTest
      ".."
      ( XB.contextParent)
    printExprTest
      "../@lang"
      (XB.contextParent </> XB.attr "lang")
    printExprTest
      """para[@type = "warning"]"""
      (XB.withPredicate
        (XB.named "para")
        (XB.attr "type" ==. "warning"))
    printExprTest
      """para[@type = "warning"][5]"""
      (XB.withPredicate
        (XB.withPredicate
          (XB.named "para")
          (XB.attr "type" ==. "warning"))
        5)
    printExprTest
      """para[5][@type = "warning"]"""
      (XB.withPredicate
        (XB.withPredicate (XB.named "para") 5)
        (XB.attr "type" ==. "warning"))
    printExprTest
      """chapter[title = "Introduction"]"""
      (XB.withPredicate
        (XB.named "chapter")
        (XB.named "title" ==. "Introduction"))
    printExprTest
      """chapter[title]"""
      (XB.withPredicate
        (XB.named "chapter")
        (XB.named "title"))
    printExprTest
      "employee[@secretary and @assistant]"
      (XB.withPredicate
        (XB.named "employee")
        (XB.attr "secretary" &&. XB.attr "assistant"))
    printExprTest
      "(foo | bar)[position() = last()]"
      (XB.withPredicate
        (XB.named "foo" |. XB.named "bar")
        (XF.position ==. XF.last))

printExprTest :: forall e eff. XB.IsExpr e => String -> e -> Test eff
printExprTest = printTest (XP.printExpr <<< XB.toExpr)

printTest :: forall a eff. (a -> String) -> String -> a -> Test eff
printTest printer expected ast =
  test expected do
    assertEqual
      { expected: expected
      , actual: printer ast
      }

main ∷ Eff (process :: PROCESS, console :: CONSOLE, assert :: ASSERT) Unit
main = moteCli (run "") suite
  where
    run indent (Plan items) = traverse_ (runItem indent) items
    runItem indent = case _ of
      Test { label, value } → do
        log $ indent <> label
        value
      Skip name →
        log $ indent <> "Skipping: " <> name
      Group { label, value } → do
        log $ indent <> label
        run (indent <> "    ") value
