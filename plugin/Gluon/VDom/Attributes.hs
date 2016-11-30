{-# LANGUAGE OverloadedStrings #-}

module Gluon.VDom.Attributes
  ( attr_
  , style_
  , class_
  , id_
  , accessKey_
  , contentEditable_
  , dir_
  , hidden_
  , lang_
  , tabIndex_
  , title_
  , type_
  , href_
  , disabled_
  , hreflang_
  , media_
  , rel_
  , name_
  , value_
  , autofocus_
  , checked_
  , height_
  , width_
  , src_
  , alt_
  , accept_
  , size_
  , placeholder_
  , rows_
  , cols_
  , readOnly_
  , required_
  , method_
  , target_
  , action_
  , encoding_
  ) where

import Protolude hiding (get, on)
import Gluon.VDom (DOMElementModifier)

-- I'm not super surw how many things we want to include here.
-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
--
-- I haven't been terribly systematic, if anything is missing use attr_

type MakeAttribute = Text -> Either (Text, Text) DOMElementModifier

attr_ :: Text -> MakeAttribute
attr_ k v = Left (k, v)

class_ :: MakeAttribute
class_ = attr_ "className"

style_ :: MakeAttribute
style_ = attr_ "style"

id_ :: MakeAttribute
id_ = attr_ "id"

accessKey_ :: MakeAttribute
accessKey_ = attr_ "accessKey"

contentEditable_ :: MakeAttribute
contentEditable_ = attr_ "contentEditable"

dir_ :: MakeAttribute
dir_ = attr_ "dir"

hidden_ :: MakeAttribute
hidden_ = attr_ "hidden"

lang_ :: MakeAttribute
lang_ = attr_ "lang"

tabIndex_ :: MakeAttribute
tabIndex_ = attr_ "tabIndex"

title_ :: MakeAttribute
title_ = attr_ "title"

type_ :: MakeAttribute
type_ = attr_ "type"

href_ :: MakeAttribute
href_ = attr_ "href"

disabled_ :: MakeAttribute
disabled_ = attr_ "disabled"

hreflang_ :: MakeAttribute
hreflang_ = attr_ "hreflang"

media_ :: MakeAttribute
media_ = attr_ "media"

rel_ :: MakeAttribute
rel_ = attr_ "rel"

name_ :: MakeAttribute
name_ = attr_ "name"

value_ :: MakeAttribute
value_ = attr_ "value"

autofocus_ :: MakeAttribute
autofocus_ = attr_ "autofocus"

checked_ :: MakeAttribute
checked_ = attr_ "checked"

height_ :: MakeAttribute
height_ = attr_ "height"

width_ :: MakeAttribute
width_ = attr_ "width"

src_ :: MakeAttribute
src_ = attr_ "src"

alt_ :: MakeAttribute
alt_ = attr_ "alt"

accept_ :: MakeAttribute
accept_ = attr_ "accept"

size_ :: MakeAttribute
size_ = attr_ "size"

placeholder_ :: MakeAttribute
placeholder_ = attr_ "placeholder"

rows_ :: MakeAttribute
rows_ = attr_ "rows"

cols_ :: MakeAttribute
cols_ = attr_ "cols"

readOnly_ :: MakeAttribute
readOnly_ = attr_ "readOnly"

required_ :: MakeAttribute
required_ = attr_ "required"

method_ :: MakeAttribute
method_ = attr_ "method"

target_ :: MakeAttribute
target_ = attr_ "target"

action_ :: MakeAttribute
action_ = attr_ "action"

encoding_ :: MakeAttribute
encoding_ = attr_ "encoding"
