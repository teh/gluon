{-# LANGUAGE RankNTypes, NoImplicitPrelude, OverloadedStrings #-}

module Gluon.VDom.Elements
  ( a_
  , abbr_
  , address_
  , area_
  , article_
  , aside_
  , audio_
  , b_
  , base_
  , bdi_
  , bdo_
  , blockquote_
  , body_
  , br_
  , button_
  , canvas_
  , caption_
  , cite_
  , code_
  , col_
  , colgroup_
  , command_
  , datalist_
  , dd_
  , del_
  , details_
  , dfn_
  , div_
  , dl_
  , dt_
  , em_
  , embed_
  , fieldset_
  , figcaption_
  , figure_
  , footer_
  , form_
  , h1_
  , h2_
  , h3_
  , h4_
  , h5_
  , h6_
  , head_
  , header_
  , hgroup_
  , hr_
  , html_
  , i_
  , iframe_
  , img_
  , input_
  , ins_
  , kbd_
  , keygen_
  , label_
  , legend_
  , li_
  , link_
  , map_
  , mark_
  , menu_
  , meta_
  , meter_
  , nav_
  , noscript_
  , object_
  , ol_
  , optgroup_
  , option_
  , output_
  , p_
  , param_
  , pre_
  , progress_
  , q_
  , rp_
  , rt_
  , ruby_
  , s_
  , samp_
  , script_
  , section_
  , select_
  , small_
  , source_
  , span_
  , strong_
  , style_
  , sub_
  , summary_
  , sup_
  , table_
  , tbody_
  , td_
  , textarea_
  , tfoot_
  , th_
  , thead_
  , time_
  , title_
  , tr_
  , track_
  , u_
  , ul_
  , var_
  , video_
  , wbr_
  -- text
  , text_
  ) where

import Protolude hiding (get, set, on)
import qualified GHC.Show as GS
import GI.WebKit2WebExtension (IsDOMNode, DOMDocument, DOMNode)

import Gluon.VDom (VNode(..))
import Gluon.VDom.Events (HandlerDescription)

type MakeNode = [Either (Text, Text) HandlerDescription] -> [VNode] -> VNode

el_ :: Text -> MakeNode
el_ tagName mods children = let (props, listeners) = partitionEithers mods in Element tagName props listeners children

-- http://w3c.github.io/html-reference/elements.html
a_ :: MakeNode
a_ = el_ "A"
abbr_ :: MakeNode
abbr_ = el_ "ABBR"
address_ :: MakeNode
address_ = el_ "address"
area_ :: MakeNode
area_ = el_ "AREA"
article_ :: MakeNode
article_ = el_ "ARTICLE"
aside_ :: MakeNode
aside_ = el_ "ASIDE"
audio_ :: MakeNode
audio_ = el_ "AUDIO"
b_ :: MakeNode
b_ = el_ "B"
base_ :: MakeNode
base_ = el_ "BASE"
bdi_ :: MakeNode
bdi_ = el_ "BDI"
bdo_ :: MakeNode
bdo_ = el_ "BDO"
blockquote_ :: MakeNode
blockquote_ = el_ "BLOCKQUOTE"
body_ :: MakeNode
body_ = el_ "BODY"
br_ :: MakeNode
br_ = el_ "BR"
button_ :: MakeNode
button_ = el_ "BUTTON"
canvas_ :: MakeNode
canvas_ = el_ "CANVAS"
caption_ :: MakeNode
caption_ = el_ "CAPTION"
cite_ :: MakeNode
cite_ = el_ "CITE"
code_ :: MakeNode
code_ = el_ "CODE"
col_ :: MakeNode
col_ = el_ "COL"
colgroup_ :: MakeNode
colgroup_ = el_ "COLGROUP"
command_ :: MakeNode
command_ = el_ "COMMAND"
datalist_ :: MakeNode
datalist_ = el_ "DATALIST"
dd_ :: MakeNode
dd_ = el_ "DD"
del_ :: MakeNode
del_ = el_ "DEL"
details_ :: MakeNode
details_ = el_ "DETAILS"
dfn_ :: MakeNode
dfn_ = el_ "DFN"
div_ :: MakeNode
div_ = el_ "DIV"
dl_ :: MakeNode
dl_ = el_ "DL"
dt_ :: MakeNode
dt_ = el_ "DT"
em_ :: MakeNode
em_ = el_ "EM"
embed_ :: MakeNode
embed_ = el_ "EMBED"
fieldset_ :: MakeNode
fieldset_ = el_ "FIELDSET"
figcaption_ :: MakeNode
figcaption_ = el_ "FIGCAPTION"
figure_ :: MakeNode
figure_ = el_ "FIGURE"
footer_ :: MakeNode
footer_ = el_ "FOOTER"
form_ :: MakeNode
form_ = el_ "FORM"
h1_ :: MakeNode
h1_ = el_ "H1"
h2_ :: MakeNode
h2_ = el_ "H2"
h3_ :: MakeNode
h3_ = el_ "H3"
h4_ :: MakeNode
h4_ = el_ "H4"
h5_ :: MakeNode
h5_ = el_ "H5"
h6_ :: MakeNode
h6_ = el_ "H6"
head_ :: MakeNode
head_ = el_ "HEAD"
header_ :: MakeNode
header_ = el_ "HEADER"
hgroup_ :: MakeNode
hgroup_ = el_ "HGROUP"
hr_ :: MakeNode
hr_ = el_ "HR"
html_ :: MakeNode
html_ = el_ "HTML"
i_ :: MakeNode
i_ = el_ "I"
iframe_ :: MakeNode
iframe_ = el_ "IFRAME"
img_ :: MakeNode
img_ = el_ "IMG"
input_ :: MakeNode
input_ = el_ "INPUT"
ins_ :: MakeNode
ins_ = el_ "INS"
kbd_ :: MakeNode
kbd_ = el_ "KBD"
keygen_ :: MakeNode
keygen_ = el_ "KEYGEN"
label_ :: MakeNode
label_ = el_ "LABEL"
legend_ :: MakeNode
legend_ = el_ "LEGEND"
li_ :: MakeNode
li_ = el_ "LI"
link_ :: MakeNode
link_ = el_ "LINK"
map_ :: MakeNode
map_ = el_ "MAP"
mark_ :: MakeNode
mark_ = el_ "MARK"
menu_ :: MakeNode
menu_ = el_ "MENU"
meta_ :: MakeNode
meta_ = el_ "META"
meter_ :: MakeNode
meter_ = el_ "METER"
nav_ :: MakeNode
nav_ = el_ "NAV"
noscript_ :: MakeNode
noscript_ = el_ "NOSCRIPT"
object_ :: MakeNode
object_ = el_ "OBJECT"
ol_ :: MakeNode
ol_ = el_ "OL"
optgroup_ :: MakeNode
optgroup_ = el_ "OPTGROUP"
option_ :: MakeNode
option_ = el_ "OPTION"
output_ :: MakeNode
output_ = el_ "OUTPUT"
p_ :: MakeNode
p_ = el_ "P"
param_ :: MakeNode
param_ = el_ "PARAM"
pre_ :: MakeNode
pre_ = el_ "PRE"
progress_ :: MakeNode
progress_ = el_ "PROGRESS"
q_ :: MakeNode
q_ = el_ "Q"
rp_ :: MakeNode
rp_ = el_ "RP"
rt_ :: MakeNode
rt_ = el_ "RT"
ruby_ :: MakeNode
ruby_ = el_ "RUBY"
s_ :: MakeNode
s_ = el_ "S"
samp_ :: MakeNode
samp_ = el_ "SAMP"
script_ :: MakeNode
script_ = el_ "SCRIPT"
section_ :: MakeNode
section_ = el_ "SECTION"
select_ :: MakeNode
select_ = el_ "SELECT"
small_ :: MakeNode
small_ = el_ "SMALL"
source_ :: MakeNode
source_ = el_ "SOURCE"
span_ :: MakeNode
span_ = el_ "SPAN"
strong_ :: MakeNode
strong_ = el_ "STRONG"
style_ :: MakeNode
style_ = el_ "STYLE"
sub_ :: MakeNode
sub_ = el_ "SUB"
summary_ :: MakeNode
summary_ = el_ "SUMMARY"
sup_ :: MakeNode
sup_ = el_ "SUP"
table_ :: MakeNode
table_ = el_ "TABLE"
tbody_ :: MakeNode
tbody_ = el_ "TBODY"
td_ :: MakeNode
td_ = el_ "TD"
textarea_ :: MakeNode
textarea_ = el_ "TEXTAREA"
tfoot_ :: MakeNode
tfoot_ = el_ "TFOOT"
th_ :: MakeNode
th_ = el_ "TH"
thead_ :: MakeNode
thead_ = el_ "THEAD"
time_ :: MakeNode
time_ = el_ "TIME"
title_ :: MakeNode
title_ = el_ "TITLE"
tr_ :: MakeNode
tr_ = el_ "TR"
track_ :: MakeNode
track_ = el_ "TRACK"
u_ :: MakeNode
u_ = el_ "U"
ul_ :: MakeNode
ul_ = el_ "UL"
var_ :: MakeNode
var_ = el_ "VAR"
video_ :: MakeNode
video_ = el_ "VIDEO"
wbr_ :: MakeNode
wbr_ = el_ "WBR"

-- Text node
text_ :: Text -> VNode
text_ = TextNode
