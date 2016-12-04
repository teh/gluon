{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}

import Protolude hiding (on, error, get)

import GI.Gtk hiding (main)
import GI.GLib (quarkFromString)
import qualified GI.Gtk as Gtk
import GI.WebKit2
import GI.Gio (memoryInputStreamNewFromData)
import Text.Sass.Compilation (compileFile)
import Data.Default (def)
import qualified Data.ByteString as BS
import System.Environment (getProgName)
import GHC.Generics (Generic)
import Options.Generic (ParseRecord, getRecord)
import System.IO (FilePath)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (copyFile)
import qualified System.FilePath as SF

data Config = Config
  { inspector :: Bool
  , scss :: [FilePath]
  , plugin :: FilePath
  } deriving (Show, Generic)

instance ParseRecord Config


renderDoc :: [FilePath] -> ByteString
renderDoc scssIncludes = "<html><head>" <> (BS.intercalate "" links) <> "</head></html>"
  where
    links = map (\path -> "<link rel=\"stylesheet\" type=\"text/css\" href=\"haskell://" <> (toS path :: ByteString) <> "\">") scssIncludes


renderSASS :: FilePath -> URISchemeRequest -> IO ()
renderSASS path request = do
  errorQuark <- quarkFromString (Just "SCSS compile error")
  scss' <- compileFile path def
  case scss' of
    Left err -> do
      print err
      gerr <- gerrorNew errorQuark 500 ("compile error: " <> (show err))
      uRISchemeRequestFinishError request gerr
    Right scssCompiled -> do
      ret <- memoryInputStreamNewFromData scssCompiled Nothing
      uRISchemeRequestFinish request ret (fromIntegral (BS.length scssCompiled)) (Just "text/css")

-- register "haskell://" data provider
registerHaskellScheme :: [FilePath] -> WebContext -> IO ()
registerHaskellScheme scssIncludes context = do
  errorQuark <- quarkFromString (Just "Haskell Scheme Error")

  webContextRegisterUriScheme context "haskell" $ \request -> do
    uri <- uRISchemeRequestGetUri request
    case uri of
      "haskell://index.html" -> do
        let doc = renderDoc scssIncludes
        ret <- memoryInputStreamNewFromData doc Nothing
        uRISchemeRequestFinish request ret (fromIntegral (BS.length doc)) (Just "text/html")
      _ -> do
        let Just filePath = map toS (T.stripPrefix "haskell://" uri)
        when (filePath `elem` scssIncludes) (renderSASS filePath request)

        unless (filePath `elem` scssIncludes) $ do
          err <- gerrorNew errorQuark 404 ("Not found: " <> uri)
          uRISchemeRequestFinishError request err


-- The long term goal would be to have a `defaultMain` that takes a
-- config file enumerating the resources. Then running an app will
-- only require the "plugin" binary. Similar to electron.
main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  -- TODO not sure how this interacts with gtk args
  config <- getRecord "Gluon main" :: IO Config
  print config

  _ <- Gtk.init (Just ((toS progName) : (map toS args)))

  win <- new Window
    [ #type := WindowTypeToplevel
    , #iconName := "applications-haskell"
    , #defaultWidth := 800
    , #defaultHeight := 600
    ]

  void $ on win #destroy mainQuit
  context <- webContextGetDefault
  registerHaskellScheme (scss config) context

  -- We copy the plugin into a temporary directory and tell webkit
  -- that our extension lives there
  withSystemTempDirectory "ext" $ \tmpPath -> do

    -- .so extension seems to be necessary
    copyFile (plugin config) (tmpPath SF.</> (SF.takeFileName (plugin config)) SF.<.> ".so")

    -- Tell webkit library where to look for our plugin code
    void $ on context #initializeWebExtensions $ do
      webContextSetWebExtensionsDirectory context (toS tmpPath)

    -- view needs to be created *after* registering haskell scheme.
    view <- new WebView []

    -- enable developer tools which allow us to attach an inspector
    when (inspector config) $ do
      settings <- webViewGetSettings view
      settingsSetEnableDeveloperExtras settings True
      webViewSetSettings view settings
      inspector <- webViewGetInspector view
      webInspectorShow inspector


    #add win view
    #loadUri view "haskell://index.html"

    #showAll win


    Gtk.main
