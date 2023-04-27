{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cards.Discover.Exe where

import Control.Applicative
import Control.Monad (filterM, guard)
import Control.Monad.State
import Data.Char
import Data.DList (DList (..))
import Data.DList qualified as DList
import Data.Foldable (for_)
import Data.List (groupBy, intercalate, sort, stripPrefix)
import Data.Maybe
import Data.String
import System.Directory
import System.FilePath
import Prelude

newtype Source = Source FilePath

newtype Destination = Destination FilePath

data AllModelsFile = AllModelsFile
  { amfModuleBase :: Module
  , amfModuleImports :: [Module]
  }

render :: Render -> String
render action = unlines $ DList.toList $ execState (unRender action) mempty

renderLine :: Render -> Render
renderLine action =
  fromString $ mconcat $ DList.toList $ execState (unRender action) mempty

newtype Render' a = Render {unRender :: State (DList String) a}
  deriving newtype
    (Functor, Applicative, Monad)

type Render = Render' ()

instance (a ~ ()) => IsString (Render' a) where
  fromString str = Render (modify (\s -> s <> pure str))

indent :: Int -> Render -> Render
indent i doc = Render do
  let new = (replicate i ' ' <>) <$> execState (unRender doc) mempty
  modify (<> new)

discoverCards :: Source -> Destination -> FilePath -> IO ()
discoverCards (Source src) (Destination dest) cardsDir = do
  let (dir, _) = splitFileName src
  files <- getFilesRecursive $ dir </> cardsDir
  let
    input =
      AllModelsFile
        { amfModuleBase = fromJust $ pathToModule src
        , amfModuleImports =
            mapMaybe
              (pathToModule . ((dir </> cardsDir) </>))
              files
        }
    output = renderFile input

  writeFile dest output

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive baseDir = sort <$> go []
 where
  go :: FilePath -> IO [FilePath]
  go dir = do
    c <-
      map (dir </>) . filter (`notElem` [".", ".."])
        <$> getDirectoryContents
          (baseDir </> dir)
    dirs <- filterM (doesDirectoryExist . (baseDir </>)) c >>= traverse go
    files <- filterM (doesFileExist . (baseDir </>)) c
    pure (files ++ concat dirs)

renderFile :: AllModelsFile -> String
renderFile amf = render do
  let modName = moduleName $ amfModuleBase amf
  renderLine do
    "{-# LINE 1 "
    fromString $ show modName
    " #-}"
  ""
  renderLine do
    "module "
    fromString modName
    " (module X) where"
  ""
  for_
    (amfModuleImports amf)
    \mod' -> renderLine do
      "import "
      fromString $ moduleName mod'
      " as X"

data Module = Module
  { moduleName :: String
  , modulePath :: FilePath
  }
  deriving stock (Eq, Show)

mkModulePieces :: FilePath -> [String]
mkModulePieces fp = do
  let extension = takeExtension fp
  guard (extension == ".hs" || extension == ".lhs")
  reverse
    . takeWhile (not . isLowerFirst)
    . reverse
    . filter noDots
    . splitDirectories
    . dropExtension
    $ fp
 where
  noDots x = "." /= x && ".." /= x

isLowerFirst :: String -> Bool
isLowerFirst [] = True
isLowerFirst (c : _) = isLower c

pathToModule :: FilePath -> Maybe Module
pathToModule file = do
  case mkModulePieces file of
    [] -> empty
    x : xs -> do
      guard $ all isValidModuleName (x : xs)
      pure (Module (intercalate "." (x : xs)) file)

isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c : cs) = isUpper c && all isValidModuleChar cs

isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

casify :: String -> String
casify str = intercalate "_" $ groupBy (\a b -> isUpper a && isLower b) str

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)
