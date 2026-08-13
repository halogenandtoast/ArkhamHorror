{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cards.Discover.Exe where

import Control.Applicative
import Control.Monad (filterM, guard)
import Control.Monad.State
import Data.Char
import Data.DList (DList (..))
import Data.DList qualified as DList
import Data.Foldable (for_)
import Data.List (elemIndex, groupBy, intercalate, isPrefixOf, nub, sort, stripPrefix)
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

instance a ~ () => IsString (Render' a) where
  fromString str = Render (modify (\s -> s <> pure str))

indent :: Int -> Render -> Render
indent i doc = Render do
  let new = (replicate i ' ' <>) <$> execState (unRender doc) mempty
  modify (<> new)

data DiscoverMode = ReExport | InstancesOnly | HomebrewContent | HomebrewCardDefs

{- | How a discovery mode that reads its inputs renders them: which module
supplies the registration helpers, which tag type and class instance to emit,
and how a declared type maps to the helper that registers it.
-}
data HomebrewSpec = HomebrewSpec
  { hsImport :: String
  , hsTypeName :: String
  , hsClassName :: String
  , hsMethodName :: String
  , hsHelper :: String -> Maybe String
  }

homebrewSpec :: DiscoverMode -> Maybe HomebrewSpec
homebrewSpec = \case
  HomebrewContent ->
    Just
      $ HomebrewSpec
        { hsImport = "Arkham.Homebrew.CardRegistry"
        , hsTypeName = "DiscoveredHomebrewCards"
        , hsClassName = "IsHomebrewCard"
        , hsMethodName = "homebrewCard"
        , hsHelper = \case
            "ActCard" -> Just "actContent"
            "AgendaCard" -> Just "agendaContent"
            "AssetCard" -> Just "assetContent"
            "EnemyCard" -> Just "enemyContent"
            "LocationCard" -> Just "locationContent"
            "StoryCard" -> Just "storyContent"
            "TreacheryCard" -> Just "treacheryContent"
            _ -> Nothing
        }
  HomebrewCardDefs ->
    Just
      $ HomebrewSpec
        { hsImport = "Arkham.Homebrew.DefsBase"
        , hsTypeName = "DiscoveredHomebrewCardDefs"
        , hsClassName = "IsHomebrewCardDefs"
        , hsMethodName = "homebrewCardDefs"
        , hsHelper = \case
            "CardDef" -> Just "cardDefEntry"
            "PlayerCardDef" -> Just "playerCardDefEntry"
            _ -> Nothing
        }
  _ -> Nothing

discoverCards :: Source -> Destination -> FilePath -> IO ()
discoverCards src dest cardsDir = discoverCardsWith src dest cardsDir Nothing ReExport

{- | 'InstancesOnly' emits @import M ()@ lines (typeclass instances in scope,
no names re-exported); an @only@ basename restricts discovery to files with
that exact name, skipping files at the scan root (so a same-named central
module never imports itself).
-}
discoverCardsWith :: Source -> Destination -> FilePath -> Maybe FilePath -> DiscoverMode -> IO ()
discoverCardsWith (Source src) (Destination dest) cardsDir only mode = do
  let (dir, _) = splitFileName src
  files <- getFilesRecursive $ dir </> cardsDir
  let
    wanted f = case only of
      Nothing -> True
      Just name -> takeFileName f == name && length (splitDirectories f) > 1
    input =
      AllModelsFile
        { amfModuleBase = fromJust $ pathToModule src
        , amfModuleImports =
            mapMaybe
              (pathToModule . ((dir </> cardsDir) </>))
              (filter wanted files)
        }
    output = case mode of
      ReExport -> renderFile input
      InstancesOnly -> renderInstancesFile input
      _ -> error "this mode is rendered after reading source files"

  case homebrewSpec mode of
    Just spec -> do
      entries <- concat <$> traverse (readHomebrewEntries spec) (amfModuleImports input)
      writeFile dest $ renderHomebrewContentFile spec (amfModuleBase input) entries
    Nothing -> writeFile dest output

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive baseDir = sort <$> go []
 where
  go :: FilePath -> IO [FilePath]
  go dir = do
    c <- map (dir </>) <$> listDirectory (baseDir </> dir)
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

renderInstancesFile :: AllModelsFile -> String
renderInstancesFile amf = render do
  let modName = moduleName $ amfModuleBase amf
  renderLine do
    "{-# LINE 1 "
    fromString $ show modName
    " #-}"
  ""
  renderLine do
    "module "
    fromString modName
    " () where"
  ""
  for_
    (amfModuleImports amf)
    \mod' -> renderLine do
      "import "
      fromString $ moduleName mod'
      " ()"

data HomebrewEntry = HomebrewEntry
  { heModule :: Module
  , heBuilder :: String
  , heHelper :: String
  }

-- Card implementation modules conventionally expose their builders with a
-- one-line signature such as @foo :: EnemyCard Foo@ (card *definition* modules
-- use @foo :: CardDef@).  Keeping this deliberately small avoids making
-- cards-discover a Haskell parser while still making an unrecognised
-- declaration fail closed (it simply is not registered).
readHomebrewEntries :: HomebrewSpec -> Module -> IO [HomebrewEntry]
readHomebrewEntries spec mod' = do
  source <- readFile $ modulePath mod'
  pure $ mapMaybe (lineEntry mod') (lines source)
 where
  lineEntry m line = do
    let stripped = dropWhile isSpace line
    guard $ not ("--" `isPrefixOf` stripped)
    let (lhs, rest) = break (== ':') stripped
    rhs <- stripPrefix "::" rest
    helper <- hsHelper spec $ takeWhile (not . isSpace) $ dropWhile isSpace rhs
    builder <- case filter (not . isSpace) lhs of
      name | validBuilder name -> Just name
      _ -> Nothing
    pure $ HomebrewEntry m builder helper

  validBuilder [] = False
  validBuilder (c : cs) = isLower c && all (\x -> isAlphaNum x || x == '_' || x == '\'') cs

renderHomebrewContentFile :: HomebrewSpec -> Module -> [HomebrewEntry] -> String
renderHomebrewContentFile HomebrewSpec {..} base entries = render do
  let modules = nub $ map heModule entries
      alias mod' = 1 + fromJust (elemIndex mod' modules)
  renderLine do
    "{-# LINE 1 "
    fromString $ show $ moduleName base
    " #-}"
  "{-# OPTIONS_GHC -Wno-unused-imports #-}"
  ""
  renderLine do
    "module "
    fromString (moduleName base)
    " where"
  ""
  renderLine do
    "import "
    fromString hsImport
  "import Arkham.Prelude"
  for_ (zip [(1 :: Int) ..] modules) \(n, mod') -> renderLine do
    "import "
    fromString $ moduleName mod'
    " qualified as Card"
    fromString $ show n
  ""
  renderLine do
    "data "
    fromString hsTypeName
  ""
  renderLine do
    "instance "
    fromString hsClassName
    " "
    fromString hsTypeName
    " where"
  indent 2 $ renderLine do
    fromString hsMethodName
    " ="
  indent 4 "mconcat"
  for_ (zip [(0 :: Int) ..] entries) \(n, HomebrewEntry {..}) -> indent 6 $ renderLine do
    if n == 0 then "[ " else ", "
    fromString heHelper
    " Card"
    fromString (show $ alias heModule)
    "."
    fromString heBuilder
  indent 6 "]"

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

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)
