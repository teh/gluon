{-# LANGUAGE OverloadedStrings #-}

module Gluon.VDom.Attributes
  ( style_
  , class_
  ) where

import Protolude hiding (get, on)
import Gluon.VDom (DOMElementModifier)

-- https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes
-- TODO

class_ :: Text -> Either (Text, Text) DOMElementModifier
class_ v = Left ("className", v)

style_ :: Text -> Either (Text, Text) DOMElementModifier
style_ v = Left ("style", v)
