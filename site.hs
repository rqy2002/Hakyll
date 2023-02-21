--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, CPP #-}
import           Data.Monoid (mappend)
import           Hakyll

import Text.Pandoc.Highlighting (Style, pygments, styleToCss)
import Text.Pandoc.Options      (ReaderOptions (..), WriterOptions (..))

import System.IO
import System.FilePath (takeFileName)
import GHC.IO.Handle (hDuplicateTo)
import Data.Aeson (encode, object, (.=), FromJSON, parseJSON, decode, withObject, (.:))
import System.Process
import Network.HTTP.Client as HTTP
import Deploy

------------------------------------------------------------------------------
-- Btex compiler
btexPort :: Int
btexPort = 1231

btexPath :: String
#ifdef WINDOWS
btexPath = "C:\\Users\\rqy\\GitRepos\\btex\\dist\\main.js"
#elif UNIX
btexPath = "/home/rqy/GitRepos/btex/dist/main.js"
#else
btexPath = error "btexPath"
#endif


data BtexResult = BtexResult { btexHtml :: String, btexData :: String, btexErrors :: [String], btexWarnings :: [String] }
instance FromJSON BtexResult where
  parseJSON = withObject "BtexResult" $ \v -> BtexResult
      <$> v .: "html"
      <*> v .: "data"
      <*> v .: "errors"
      <*> v .: "warnings"

btexToHtml :: BtexResult -> String
btexToHtml (BtexResult html dataa errors warnings) =
  concatMap (\t -> "<div class=\"error-message\"><b>错误 </b>" ++ t ++ "</div>\n") errors ++
  concatMap (\t -> "<div class=\"warning-message\"><b>警告 </b>" ++ t ++ "</div>\n") warnings ++
  html

btexCompiler :: Compiler (Item String)
btexCompiler = getResourceBody >>= withItemBody (\content -> do
  let reqBody = object [ "code" .= content ]
  let request = defaultRequest {
        HTTP.host = "127.0.0.1",
        HTTP.port = btexPort,
        method = "POST",
        requestBody = RequestBodyLBS $ encode reqBody }
  unsafeCompiler $ do
    manager <- newManager defaultManagerSettings
    response <- httpLbs request manager
    let result = decode (responseBody response) :: Maybe BtexResult
    case result of
      Nothing -> return ""
      Just s -> return $ btexToHtml s)

------------------------------------------------------------------------------
pandocCodeStyle :: Style
pandocCodeStyle = pygments

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle   = Just pandocCodeStyle
      }

hakyllConfiguration :: Configuration
hakyllConfiguration = defaultConfiguration
                        { deploySite = deploy }

hakyllMain :: IO ()
hakyllMain = hakyllWith hakyllConfiguration $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/fonts/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "Hugo/public/**" $ do
    route   $ gsubRoute "Hugo/public/" (const "OI/")
    compile copyFileCompiler

  match "favicons/*" $ do
    route   $ gsubRoute "favicons/" (const "")
    compile copyFileCompiler

  match "about.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler'
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      -- >>= relativizeUrls

  match "contact.html" $ do
    route   idRoute
    compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        -- >>= relativizeUrls

  match "posts/*" $ version "raw" $ do
    route idRoute
    compile getResourceBody

  match "posts/*.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler'
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      -- >>= relativizeUrls

  match "posts/*.btex" $ do
    route $ setExtension "html"
    compile $ btexCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      -- >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
      let archiveCtx = listField "posts" postCtx (return posts) `mappend`
                       constField "title" "归档" `mappend`
                       defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        -- >>= relativizeUrls


  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
      let indexCtx = listField "posts" postCtx (return posts) `mappend`
                     defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        -- >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ do
      makeItem $ styleToCss pandocCodeStyle

  match "friends.html" $ do
    route idRoute
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      -- >>= relativizeUrls

-- From `silently`
mNullDevice :: FilePath
#ifdef WINDOWS
mNullDevice = "\\\\.\\NUL"
#elif UNIX
mNullDevice = "/dev/null"
#else
mNullDevice = error "null device"
#endif

main :: IO ()
main = withFile mNullDevice WriteMode $ \devnull ->
  withCreateProcess ((proc "node" [btexPath, "-p", show btexPort]) { std_in = NoStream, std_out = CreatePipe }) $ \_ (Just outp) _ _ -> do
  hSetBuffering stdout LineBuffering -- change buffering to line buffering to work correctly in emacs shell
  putStr "Starting btex server... "
  content <- hGetLine outp
  putStrLn "Done"
  hDuplicateTo outp devnull
  hakyllMain

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  mapContext takeFileName (pathField "source") `mappend`
  mapContext toUrl (pathField "source_url") `mappend`
  dateField "date" "%Y 年 %m 月 %e 日" `mappend`
  defaultContext
