{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}

import Protolude hiding (on, error, get)

import GI.Gtk hiding (main)
import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import GI.WebKit2
import GI.Gio (memoryInputStreamNewFromData)

import System.Environment (getProgName)


main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs

  _ <- Gtk.init (Just ((toS progName) : (map toS args)))

  win <- new Window
    [ #type := WindowTypeToplevel
    , #iconName := "applications-haskell"
    ]

  on win #destroy mainQuit
  context <- webContextGetDefault
  on context #initializeWebExtensions $ do
    print "initializeWebExtensions"
    webContextSetWebExtensionsDirectory context "./ext"

  view <- new WebView []

  -- register haskell scheme
  context <- webContextGetDefault
  webContextRegisterUriScheme context "haskell" $ \request -> do
    path <- uRISchemeRequestGetUri request
    ret <- memoryInputStreamNewFromData "" Nothing
    print path
    uRISchemeRequestFinish request ret 14 (Just "text/html")

  #add win view
  #loadUri view "haskell://test"


  #showAll win

  Gtk.main
