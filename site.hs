--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, CPP #-}
import           Data.Monoid (mappend)
import           Hakyll

import Text.Pandoc.Highlighting (Style, pygments, styleToCss)
import Text.Pandoc.Options      (ReaderOptions (..), WriterOptions (..))
import Text.Pandoc.Extensions

import System.IO
import System.FilePath (takeFileName, (</>), takeDirectory, dropExtension)
import GHC.IO.Handle (hDuplicateTo)
import Data.Aeson (encode, object, (.=), FromJSON, parseJSON, decode, withObject, (.:))
import System.Process
import Network.HTTP.Client as HTTP
import Deploy
import Text.HTML.TagSoup (Tag (..), parseTags, renderTags)
import Text.HTML.TagSoup as TS
import Control.Monad (forM, mapM)

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
#error "need -DWINDOWS or -DUNIX"
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

-- Move the toc up the btex-output
btexMoveOutToc :: String -> String
btexMoveOutToc str = let (t1, t2, t3) = findToc $ parseTags str in TS.renderTags (t2 ++ t1 ++ t3) where
  isTocOpen (TagOpen "div" l) = ("class", "toc") `elem` l
  isTocOpen _ = False
  elemLevel (TagOpen _ _) = 1
  elemLevel (TagClose _) = -1
  elemLevel _ = 0
  findToc tags = findToc' $ break isTocOpen tags
  findToc'  (pre, (tocbegin : last)) =
    let taglist = zip last $ scanl1 (+) $ map elemLevel last in
    let (mid, ((tocend, _) : post)) = break (\(_, k) -> k < 0) taglist in
      (pre, [tocbegin] ++ map fst mid ++ [tocend], map fst post)
  findToc' (u, []) =  (u, [], [])

btexCompiler :: Compiler (Item String)
btexCompiler = getResourceBody >>= withItemBody (\content -> do
  id <- getUnderlying
  preamble <- getMetadataField id "preamble"
  preambleContent <- mapM (loadBody . preambleToPath id) preamble :: Compiler (Maybe String)

  unsafeCompiler $ do
    let reqBody = case preambleContent of
                    Just p -> object [ "code" .= content, "preamble" .= p ]
                    Nothing -> object [ "code" .= content ]
    let request = defaultRequest {
          HTTP.host = "127.0.0.1",
          HTTP.port = btexPort,
          method = "POST",
          requestBody = RequestBodyLBS $ encode reqBody }
    manager <- newManager defaultManagerSettings
    response <- httpLbs request manager
    let result = decode (responseBody response) :: Maybe BtexResult
    case result of
      Nothing -> return ""
      Just s -> return $ btexMoveOutToc $ btexToHtml s)
  where
    preambleToPath id p = fromFilePath $ (takeDirectory $ toFilePath id) </> "preamble" </> (dropExtension p ++ ".btex")

------------------------------------------------------------------------------
pandocCodeStyle :: Style
pandocCodeStyle = pygments

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
      { readerExtensions = disableExtension Ext_tex_math_dollars $
                            disableExtension Ext_latex_macros $
                            disableExtension Ext_raw_tex $
                            disableExtension Ext_subscript $
                            disableExtension Ext_superscript $
                            disableExtension Ext_smart $
                            readerExtensions defaultHakyllReaderOptions
      }
    defaultHakyllWriterOptions
      { writerHighlightStyle   = Just pandocCodeStyle
      }

hakyllConfiguration :: Configuration
hakyllConfiguration = defaultConfiguration
                        { deploySite = deploy }

resourceRules :: Rules ()
resourceRules = do
  match (foldl1 (.||.) ["images/*", "js/*", "css/fonts/*", "tex/*.pdf", "old_posts/*/*.svg"]) $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "favicons/*" $ do
    route   $ gsubRoute "favicons/" (const "")
    compile copyFileCompiler

postRules :: Rules ()
postRules = do
  match ("posts/*.md" .||. "old_posts/*.md") $ do
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

  match "posts/preamble/*.btex" $ compile getResourceBody

hakyllMain :: IO ()
hakyllMain = hakyllWith hakyllConfiguration $ do
  resourceRules
  postRules

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

  -- match "posts/*" $ version "raw" $ do
  --   route idRoute
  --   compile getResourceBody

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      oldposts <- recentFirst =<< loadAll "old_posts/*"
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
      let archiveCtx = listField "posts" postCtx (return posts) `mappend`
                       listField "old_posts" postCtx (return oldposts) `mappend`
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
      let indexCtx = listField "posts" postCtx (return $ take 10 posts) `mappend`
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
#error "need -DWINDOWS or -DUNIX"
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
  dateField "date" "%Y 年 %m 月 %e 日" `mappend`
  defaultContext
