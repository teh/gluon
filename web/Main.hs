{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}

import Protolude hiding (on, error, get)

import GI.Gtk hiding (main)
import qualified GI.Gtk as Gtk
import GI.WebKit2
import GI.Gio (memoryInputStreamNewFromData)
import Text.Sass.Compilation (compileFile)
import Data.Default (def)
import qualified Data.ByteString as BS
import System.Environment (getProgName)

mainDoc :: ByteString
mainDoc = "<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"haskell://bootstrap.css\"></head></html>"

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs

  _ <- Gtk.init (Just ((toS progName) : (map toS args)))

  win <- new Window
    [ #type := WindowTypeToplevel
    , #iconName := "applications-haskell"
    ]

  void $ on win #destroy mainQuit
  context <- webContextGetDefault
  void $ on context #initializeWebExtensions $ do
    webContextSetWebExtensionsDirectory context "./ext"

  -- register "haskell://" data provider
  webContextRegisterUriScheme context "haskell" $ \request -> do
    uri <- uRISchemeRequestGetUri request
    case uri of
      "haskell://bootstrap.css" -> do
        scss <- compileFile "/home/tom/src/bootstrap/scss/bootstrap.scss" def
        case scss of
          Left err -> print err
          Right scssCompiled -> do
            ret <- memoryInputStreamNewFromData scssCompiled Nothing
            uRISchemeRequestFinish request ret (fromIntegral (BS.length scssCompiled)) (Just "text/css")
      "haskell://test" -> do
        ret <- memoryInputStreamNewFromData mainDoc Nothing
        uRISchemeRequestFinish request ret (fromIntegral (BS.length mainDoc)) (Just "text/html")

  view <- new WebView []

  -- enable developer tools which allow us to attach an inspector
  settings <- webViewGetSettings view
  settingsSetEnableDeveloperExtras settings True
  webViewSetSettings view settings

  -- show web inspector after showall. Probably bettero to tie that to
  -- a button and/or key combo.
  inspector <- webViewGetInspector view
  webInspectorShow inspector

  #add win view
  #loadUri view "haskell://test"

  #showAll win


  Gtk.main
