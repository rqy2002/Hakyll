module Deploy (deploy) where

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import System.Exit (ExitCode)
import System.Process (system)
import Hakyll.Core.Util.File (getRecursiveContents)
import Hakyll.Web.Html (withUrls, isExternal, toSiteRoot)
import qualified Data.ByteString as B (readFile)
import System.FilePath ((</>), takeExtension, dropExtension, takeDirectory,
                       makeRelative)
import System.Directory (createDirectoryIfMissing, copyFile)
import qualified Data.Map as M
import Data.Hashable (hashWithSalt)
import Numeric (showIntAtBase)
import Data.Char (chr)
import Data.List (isPrefixOf, intercalate)

theAnswerToTheUniverse :: Int
theAnswerToTheUniverse = 42

staticFileExtensions :: [String]
staticFileExtensions = [".js", ".css", ".png", ".jpg", -- ".woff", ".woff2"
                        ".jpeg", ".svg", ".lrc", ".ogg", ".gif"]

urlFileExtensions :: [String]
urlFileExtensions = [".html", ".xml"]

insertHash :: FilePath -> FilePath -> IO FilePath
insertHash top file
  | takeExtension file `elem` staticFileExtensions = do
    content <- B.readFile (top </> file)
    let hashInt = hashWithSalt theAnswerToTheUniverse content
    let hashUnsigned = fromIntegral hashInt + if hashInt < 0 then intRange else 0
    let hashStr = showIntAtBase 36 base36 hashUnsigned ""
    return $ dropExtension file ++ "_" ++ hashStr ++ takeExtension file
  | otherwise = return file
    where
      intRange = - 2 * fromIntegral (minBound :: Int)
      base36 i = chr (i + if i < 10 then 48 else 87)

getStaticFileMaps :: FilePath -> IO (M.Map FilePath FilePath)
getStaticFileMaps top = do
  content <- getRecursiveContents (\_ -> return False) top
  newContent <- mapM (\f -> ((,) f) <$> insertHash top f) content
  return $ M.fromList newContent

modifyUrls :: M.Map FilePath FilePath -> FilePath -> String -> String
modifyUrls files file = withUrls modify where
  dir = takeDirectory file
  r = toSiteRoot file
  isAbs x = "/" `isPrefixOf` x && not ("//" `isPrefixOf` x)
  modify url
    | isAbs url = case (M.lookup (tail url) files) of
                    Nothing -> r </> tail url
                    Just url' -> r </> url'
    | isExternal url = url
    | otherwise = case (M.lookup (dir </> url) files) of
                    Nothing -> url
                    Just url' -> makeRelative dir url'

genDeployFiles :: FilePath -> FilePath -> (FilePath -> IO Bool) -> IO ()
genDeployFiles top deployTop ignore = do
  files <- getStaticFileMaps top
  mapM_ (genDeployFile top deployTop files) $ M.assocs files
  where
    genDeployFile top deployTop files (file, file') = do
      let transform = takeExtension file `elem` urlFileExtensions
      ignored <- ignore file
      if transform && not ignored
        then do
          putStrLn $ "Transfroming " ++ (top </> file) ++ " to " ++ (deployTop </> file') ++ "."
          content <- readFile (top </> file)
          let content' = modifyUrls files file content
          createDirectoryIfMissing True (takeDirectory (deployTop </> file'))
          writeFile (deployTop </> file') content'
        else do
          putStrLn $ "Copying " ++ (top </> file) ++ " to " ++ (deployTop </> file') ++ "."
          createDirectoryIfMissing True (takeDirectory (deployTop </> file'))
          copyFile (top </> file) (deployTop </> file')

deployDir :: String
deployDir = "_deploy/"

ignoreFiles :: FilePath -> IO Bool
ignoreFiles = return . isPrefixOf "OI/"

repoUrl :: String
repoUrl = "git@github.com:MenciStaticSites/rqy-blog.git"

deployGitCommands :: String
deployGitCommands = intercalate " && "
                      [ "cd " ++ deployDir,
                        "git init",
                        "git add -A",
                        "git commit -m \"$(date +%F-%H:%M:%S)\"",
                        "git push -u " ++ repoUrl ++ " HEAD:main --force"
                      ]

deploy :: Configuration -> IO ExitCode
deploy configuration = do
  genDeployFiles (destinationDirectory configuration) deployDir ignoreFiles
  system deployGitCommands
