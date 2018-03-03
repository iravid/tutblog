{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (filterM, forM, forM_, join, foldM)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  (intercalate, isSuffixOf)
import           Data.List.Split            (splitOn)
import qualified Data.Map.Lazy as Map
import           Data.Maybe                 (listToMaybe, fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TI
import           Options.Applicative
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist, listDirectory)
import           System.Environment         (getArgs)
import           System.FilePath            (takeBaseName, (</>))
import           System.IO                  (stdin, stdout, stderr, hPutStrLn)
import           System.IO.Temp             (withTempDirectory)
import           System.Process.Typed
import           Text.Pandoc
import           Text.Pandoc.Definition     (Block)
import           Text.Pandoc.Templates      (getDefaultTemplate)
import           Text.Pandoc.Walk           (walk)
import           Text.Regex.TDFA


data Config = Config { coursierLauncherPath :: CoursierLauncherPath }

data ParsedMdFile = ParsedMdFile { mdData :: T.Text
                                 , coursierDeps :: [CoursierDep]
                                 , scalaVersion :: ScalaVersion }

data TypecheckedMd = TypecheckedMd { content :: T.Text
                                   , filename :: FilePath }

newtype ScalaVersion = ScalaVersion { unScalaVersion :: String }
newtype OrgDirectory = OrgDirectory { unOrgDir :: FilePath }
newtype TutDirectory = TutDirectory { unTutDir :: FilePath }
newtype MdDirectory = MdDirectory { unMdDir :: FilePath }
newtype CoursierLauncherPath = CoursierLauncherPath { unLauncherPath :: FilePath }

newtype CoursierDep = CoursierDep { unCoursierDep :: String }
newtype ClasspathEntry = ClasspathEntry { unClasspathEntry :: String }

configParser :: Parser Config
configParser =
  Config <$> CoursierLauncherPath <$> strOption
      (  long "coursier-launcher"
      <> metavar "COURSIER"
      <> help "Path to coursier launcher" )

logger :: String -> IO ()
logger = hPutStrLn stderr

main :: IO ()
main = do
  config <- execParser $ info configParser fullDesc
  inputContents <- TI.hGetContents stdin
  parsedMd <- transformOrgFile inputContents
  -- parsedMds <- transformOrgFiles $ orgDirectory config
  typechecked <- typecheckMd (coursierLauncherPath config) parsedMd
  TI.hPutStr stdout (content typechecked)
  -- forM_ parsedMds $ \parsedMd -> do
  --   typechecked <- typecheckMd (coursierLauncherPath config) parsedMd
  --   TI.writeFile (filename typechecked) (content typechecked)

-- transformOrgFiles :: OrgDirectory -> IO [ParsedMdFile]
-- transformOrgFiles (OrgDirectory orgDir) = do
--   files <- getOrgFiles orgDir
--   forM files $ \f -> transformOrgFile (orgDir </> f)
--   where
--     getOrgFiles :: FilePath -> IO [FilePath]
--     getOrgFiles path = do
--       dirEntries <- listDirectory path
--       onlyFiles <- filterM (doesFileExist . (path </>)) dirEntries
--       return $ filter (isSuffixOf ".org") onlyFiles

transformOrgFile :: T.Text -> IO ParsedMdFile
transformOrgFile contents = do
  parsed@(Pandoc orgMeta _) <- parseOrgToPandoc contents
  logger $ "Meta: " <> (show $ unMeta orgMeta)
  md <- pandocToMarkdown parsed
  let scalaVersion = extractScalaVersion $ fromMaybe (MetaString "2.12.4") . Map.lookup "scala_version" . unMeta $ orgMeta
      deps = extractScalaDeps $ fromMaybe (MetaList []) . Map.lookup "scala_deps" . unMeta $ orgMeta
  logger $ "Scala version: " <> (show scalaVersion)
  logger $ "Deps: " <> (show $ fmap unCoursierDep deps)
  return ParsedMdFile { mdData = md
                      , coursierDeps = deps
                      , scalaVersion = ScalaVersion scalaVersion }
  where
    parseOrgToPandoc :: T.Text -> IO Pandoc
    parseOrgToPandoc text = runIOorExplode $ readOrg def text

    extractScalaVersion :: MetaValue -> String
    extractScalaVersion (MetaString scalaVersion) = scalaVersion
    extractScalaVersion _                         = "2.12.4"

    extractScalaDeps :: MetaValue -> [CoursierDep]
    extractScalaDeps (MetaString dep) = fmap CoursierDep (splitOn "," dep)
    extractScalaDeps _                = []

pandocToMarkdown :: Pandoc -> IO T.Text
pandocToMarkdown pandoc = runIOorExplode $ do
  gfmTemplate <- getDefaultTemplate "gfm"
  md <- writeMarkdown (markdownOpts gfmTemplate) (walk fixTutCodeBlocks pandoc)
  return $ T.unlines . fmap replaceCodeFence . T.lines $ md
  where
    markdownOpts :: String -> WriterOptions
    markdownOpts template =
      def { writerExtensions = (enableExtension Ext_yaml_metadata_block githubMarkdownExtensions)
          , writerTemplate = Just template }

    replaceCodeFence :: T.Text -> T.Text
    replaceCodeFence line =
      let stringLine = T.unpack line
          (_, _, _, matchedGroups) = (stringLine =~ ("``` ([a-zA-Z:]+)" :: String) :: (String, String, String, [String])) in
        case matchedGroups of
          (codeSpec:_) -> T.pack $ "```" <> codeSpec
          _            -> line

    fixTutCodeBlocks :: Block -> Block
    fixTutCodeBlocks c@(CodeBlock (ident, classes, mods) text) =
      if (any (== "tut") classes && (not $ null mods))
      then (CodeBlock (ident, ["tut:" <> concatenatedMods], []) text)
      else c
      where
        modKeys = fst <$> mods
        concatenatedMods = intercalate ":" modKeys
    fixTutCodeBlocks otherwise                  = otherwise

typecheckMd :: CoursierLauncherPath -> ParsedMdFile -> IO TypecheckedMd
typecheckMd coursierPath parsedMd = do
  logger $ "Typechecking file"
  withTempDirectory "/tmp" "tutblog" $ \tempdir -> do
    let tutDir = tempdir </> "tut"
        outputDir = tempdir </> "output"
        mdFilename = "file.md"
    logger $ "Running in " <> tempdir
    createDirectoryIfMissing True tutDir
    createDirectoryIfMissing True outputDir
    writeTutFile (TutDirectory tutDir) mdFilename (mdData parsedMd)
    runTut coursierPath (TutDirectory tutDir) (MdDirectory outputDir) (scalaVersion parsedMd) (coursierDeps parsedMd)
    typecheckedContents <- TI.readFile $ outputDir </> mdFilename
    return $ TypecheckedMd typecheckedContents mdFilename
  where
    writeTutFile :: TutDirectory -> FilePath -> T.Text -> IO ()
    writeTutFile (TutDirectory tutDir) destFilename contents = do
      createDirectoryIfMissing True tutDir
      TI.writeFile (tutDir </> destFilename) contents
      logger $ "Wrote " <> (tutDir </> destFilename)

runTut :: CoursierLauncherPath -> TutDirectory -> MdDirectory -> ScalaVersion -> [CoursierDep] -> IO ()
runTut coursier (TutDirectory tutDir) (MdDirectory mdDir) (ScalaVersion scalaVer) deps = do
  let args = [ "launch", "-r", "https://dl.bintray.com/tpolecat/maven/", "org.tpolecat::tut-core:0.6.3"
             , "-e", scalaVer
             , "--"
             , tutDir
             , mdDir
             , ".*\\.md$" ]

  depEntries <- join <$> forM deps (coursierGenClasspath coursier)
  let classpathArgs = ["-classpath", intercalate ":" $ unClasspathEntry <$> depEntries]
  runProcess_ $ tutProcess coursier args classpathArgs
  where
    tutProcess coursier args classpathArgs =
      setStdout (useHandleOpen stderr)
      $ setStderr (useHandleOpen stderr)
      $ proc (unLauncherPath coursier) (args <> classpathArgs)

coursierGenClasspath :: CoursierLauncherPath -> CoursierDep -> IO [ClasspathEntry]
coursierGenClasspath coursier (CoursierDep dep) = do
  (out, err) <- readProcess_ (proc (unLauncherPath coursier) ["fetch", "-r", "central" , "-r", "bintray:bintray/jcenter", "-p", dep])
  let entries = filter (not . BS.null) $ BS.split '\n' out
      stripped = BS.dropWhile (== '\n') . BS.reverse . BS.dropWhile (== '\n') . BS.reverse <$> entries
  return $ ClasspathEntry . BS.unpack <$> entries
