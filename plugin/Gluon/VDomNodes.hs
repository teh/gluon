{-# LANGUAGE RankNTypes, NoImplicitPrelude, OverloadedStrings #-}

module Gluon.VDomNodes
  ( div_
  , p_
  , text_
  , class_
  , style_
  ) where

import Protolude hiding (get, set, on)
import qualified GHC.Show as GS
import GI.WebKit2WebExtension (IsDOMNode, DOMDocument, DOMNode)

import Gluon.VDom (VNode(..), DOMElementModifier)

type MakeNode = [Either (Text, Text) DOMElementModifier] -> [VNode] -> VNode

el_ :: Text -> MakeNode
el_ tagName mods children = let (props, listeners) = partitionEithers mods in Element tagName props listeners children

div_ :: MakeNode
div_ = el_ "DIV"

p_ :: MakeNode
p_ = el_ "DIV"


text_ :: Text -> VNode
text_ = TextNode


--- attributes, maybe in a different module?

class_ :: Text -> Either (Text, Text) DOMElementModifier
class_ v = Left ("className", v)

style_ :: Text -> Either (Text, Text) DOMElementModifier
style_ v = Left ("style", v)
