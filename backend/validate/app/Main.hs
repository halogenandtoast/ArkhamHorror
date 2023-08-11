{-# LANGUAGE TupleSections #-}

module Main where

import ClassyPrelude hiding (throwIO)

import Arkham.Asset
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (SomeAssetCard (..), assetHealth, assetSanity)
import Arkham.Card
import Arkham.Card.Cost
import Arkham.ClassSymbol
import Arkham.Classes.Entity
import Arkham.EncounterCard
import Arkham.EncounterSet
import Arkham.Enemy
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (
  SomeEnemyCard (..),
  enemyEvade,
  enemyFight,
  enemyHealth,
  enemyHealthDamage,
  enemySanityDamage,
 )
import Arkham.Event
import Arkham.Event.Cards qualified as Events
import Arkham.GameValue
import Arkham.Location
import Arkham.Location.Types (
  SomeLocationCard (..),
  locationRevealClues,
  locationShroud,
 )
import Arkham.Name
import Arkham.PlayerCard
import Arkham.Skill
import Arkham.Skill.Cards qualified as Skills
import Arkham.SkillType hiding (allSkills)
import Arkham.Trait hiding (Dunwich)
import Arkham.Treachery
import Arkham.Treachery.Cards qualified as Treacheries
import Control.Monad.Random.Lazy hiding (mapM_)
import Control.Monad.Validate
import Data.Aeson
import Data.Text qualified as T
import System.Directory
import Text.Read (readEither)

data CardJson = CardJson
  { code :: CardCode
  , name :: Text
  , back_name :: Maybe Text
  , subname :: Maybe Text
  , cost :: Maybe Int
  , exceptional :: Bool
  , is_unique :: Bool
  , traits :: Maybe Text
  , skill_agility :: Maybe Int
  , skill_combat :: Maybe Int
  , skill_intellect :: Maybe Int
  , skill_willpower :: Maybe Int
  , skill_wild :: Maybe Int
  , faction_name :: String
  , faction2_name :: Maybe String
  , victory :: Maybe Int
  , enemy_fight :: Maybe Int
  , health :: Maybe Int
  , sanity :: Maybe Int
  , health_per_investigator :: Maybe Bool
  , enemy_evade :: Maybe Int
  , enemy_damage :: Maybe Int
  , enemy_horror :: Maybe Int
  , clues :: Maybe Int
  , shroud :: Maybe Int
  , xp :: Maybe Int
  , quantity :: Int
  , type_code :: String
  , pack_code :: String
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

newtype UnknownCard = UnknownCard CardCode
  deriving stock (Show)

data MissingImplementation = MissingImplementation CardCode Name
  deriving stock (Show)

data NameMismatch = NameMismatch CardCode Name Name
  deriving stock (Show)

data QuantityMismatch = QuantityMismatch CardCode Name Int (Maybe Int)
  deriving stock (Show)

data UniqueMismatch = UniqueMismatch CardCode Name
  deriving stock (Show)

data CardCostMismatch
  = CardCostMismatch
      CardCode
      Name
      (Maybe Int)
      (Maybe CardCost)
  deriving stock (Show)

data VictoryMismatch = VictoryMismatch CardCode Name (Maybe Int) (Maybe Int)
  deriving stock (Show)

data XpMismatch = XpMismatch CardCode Name Int Int
  deriving stock (Show)

data EnemyStatsMismatch
  = EnemyStatsMismatch
      CardCode
      Name
      (Int, GameValue, Maybe Int)
      (Int, GameValue, Maybe Int)
  deriving stock (Show)

data AssetStatsMismatch
  = AssetStatsMismatch
      CardCode
      Name
      (Maybe Int, Maybe Int)
      (Maybe Int, Maybe Int)
  deriving stock (Show)

data EnemyDamageMismatch
  = EnemyDamageMismatch
      CardCode
      Name
      (Int, Int)
      (Int, Int)
  deriving stock (Show)

data ClassMismatch = ClassMismatch CardCode Name String (Set ClassSymbol)
  deriving stock (Show)

data SkillsMismatch = SkillsMismatch CardCode Name [SkillIcon] [SkillIcon]
  deriving stock (Show)

data TraitsMismatch
  = TraitsMismatch
      CardCode
      Name
      (Set Trait)
      (Set Trait)
  deriving stock (Show)

data ShroudMismatch = ShroudMismatch CardCode Name Int Int
  deriving stock (Show)

data ClueMismatch = ClueMismatch CardCode Name Int Int
  deriving stock (Show)

instance Exception UnknownCard
instance Exception MissingImplementation
instance Exception NameMismatch
instance Exception UniqueMismatch
instance Exception CardCostMismatch
instance Exception ClassMismatch
instance Exception SkillsMismatch
instance Exception TraitsMismatch
instance Exception VictoryMismatch
instance Exception EnemyStatsMismatch
instance Exception AssetStatsMismatch
instance Exception EnemyDamageMismatch
instance Exception ShroudMismatch
instance Exception ClueMismatch
instance Exception XpMismatch
instance Exception QuantityMismatch

filterTest :: [(CardCode, CardDef)] -> [(CardCode, CardDef)]
filterTest =
  filter
    ( \(code, cdef) ->
        code /= "asset"
          && cdEncounterSet cdef /= Just Test
          && not
            ("b" `isSuffixOf` unCardCode code)
    )

filterTestEntities :: [(CardCode, a)] -> [(CardCode, a)]
filterTestEntities =
  filter
    ( \(code, _) ->
        code `notElem` ["enemy", "location", "asset"]
          && not
            ("b" `isSuffixOf` unCardCode code)
    )

toClassSymbol :: String -> Maybe ClassSymbol
toClassSymbol = \case
  "Guardian" -> Just Guardian
  "Seeker" -> Just Seeker
  "Survivor" -> Just Survivor
  "Rogue" -> Just Rogue
  "Mystic" -> Just Mystic
  "Neutral" -> Just Neutral
  _ -> Nothing

normalizeName :: CardCode -> Text -> Text
normalizeName "01121a" _ = "Predator or Prey?"
normalizeName "02219" _ = "Powder of Ibn-Ghazi"
-- TODO: update these names
normalizeName "03095" _ = "Maniac"
normalizeName "03096" _ = "Young Psychopath"
normalizeName "03184" _ = "Mad Patient"
normalizeName _ a = T.replace "\8217" "'" a

normalizeSubname :: CardCode -> Maybe Text -> Maybe Text
normalizeSubname "02230" _ = Just "... Or Are They?"
normalizeSubname "03182a" _ = Just "He's Not Doing All Too Well"
normalizeSubname _ a = T.replace "\8217" "'" <$> a

normalizeCost :: CardCode -> Maybe Int -> Maybe CardCost
normalizeCost "02178" _ = Nothing
normalizeCost _ (Just (-2)) = Just DynamicCost
normalizeCost _ (Just n) = Just (StaticCost n)
normalizeCost _ Nothing = Nothing

allCards :: Map CardCode CardDef
allCards = allPlayerCards <> allEncounterCards

getSkills :: CardJson -> [SkillIcon]
getSkills CardJson {..} =
  getSkill (SkillIcon SkillWillpower) skill_willpower
    <> getSkill (SkillIcon SkillIntellect) skill_intellect
    <> getSkill (SkillIcon SkillCombat) skill_combat
    <> getSkill (SkillIcon SkillAgility) skill_agility
    <> getSkill WildIcon skill_wild
 where
  getSkill _ Nothing = []
  getSkill skillType (Just n) = replicate n skillType

getTraits :: CardJson -> Set Trait
getTraits CardJson {code} | code == "01000" = mempty
getTraits CardJson {..} = case traits of
  Nothing -> mempty
  Just s -> setFromList $ map toTrait (T.splitOn ". " $ cleanText s)
 where
  toTrait x =
    handleEither x
      . readEither
      . T.unpack
      . normalizeTrait
      . cleanText
      $ T.replace "á" "a"
      $ T.replace "-" ""
      $ T.replace " " "" x
  handleEither _ (Right a) = a
  handleEither x (Left err) =
    error $ show code <> ": " <> err <> " " <> show x <> " from " <> show traits
  normalizeTrait "Human" = "Humanoid"
  normalizeTrait "Possessed" = "Lunatic"
  normalizeTrait x = x
  cleanText = T.dropWhileEnd (\c -> c == '.' || c == ' ')

toGameVal :: Bool -> Int -> GameValue
toGameVal True n = PerPlayer n
toGameVal False n = Static n

normalizeCardCode :: CardCode -> CardCode
normalizeCardCode "01121" = "01121a"
normalizeCardCode "03076a" = "03076"
normalizeCardCode "03221" = "03221b"
normalizeCardCode "03323" = "03323a"
normalizeCardCode "04128" = "04128a"
normalizeCardCode "04133" = "04133a"
normalizeCardCode "04126" = "04126a"
normalizeCardCode "50026" = "50026a"
normalizeCardCode c = c

runMissing :: Maybe Text -> Map CardCode CardJson -> IO ()
runMissing mPackCode cards = do
  let
    cardCodes =
      map CardCode
        . sort
        . map (unCardCode . normalizeCardCode)
        . toList
        $ keysSet (filterOutIrrelevant mPackCode cards)
          `difference` keysSet allCards
  case filter (not . ignoreCardCode) cardCodes of
    [] -> putStrLn "Complete!"
    xs -> do
      print xs
      for_ (mapMaybe (`lookup` cards) xs) $ \card -> do
        let
          suffix =
            maybe
              ""
              (\level -> if level == 0 then "" else "(" <> tshow level <> ")")
              (xp card)
        putStrLn $ unCardCode (code card) <> ": " <> name card <> suffix

filterOutIrrelevant
  :: Maybe Text -> Map CardCode CardJson -> Map CardCode CardJson
filterOutIrrelevant mPackCode =
  filterMap
    ( \card ->
        (code card /= "01000")
          && ( type_code card `notElem` ["investigator", "scenario", "act", "agenda"]
             )
          && maybe True ((== pack_code card) . unpack) mPackCode
    )

normalizeClassSymbol :: Maybe ClassSymbol -> Maybe ClassSymbol
normalizeClassSymbol (Just Mythos) = Nothing
normalizeClassSymbol c = c

normalizeEnemyStats :: CardCode -> (Int, GameValue, Maybe Int) -> (Int, GameValue, Maybe Int)
normalizeEnemyStats "05085" (fight, _, evade) = (fight, Static 3, evade)
normalizeEnemyStats _ stats = stats

ignoreCardCode :: CardCode -> Bool
ignoreCardCode x = T.isPrefixOf "x" (unCardCode x) || x `elem` ignoredCardCodes
 where
  ignoredCardCodes =
    [ "03076" -- Constance Dumaine
    , "03221a" -- The Organist
    , "03325c" -- Shores of Hali
    , "03326d" -- Bleak Plains
    , "03326e" -- Bleak Plains
    , "03327c" -- Inhabitant of Carcosa
    , "03327e" -- A Moment's Rest
    , "03327f" -- The Coffin
    , "03327g" -- The Coffin
    , "03328d" -- The King's Parade
    , "03328e" -- The King's Parade
    , "03328f" -- The King's Parade
    , "03328g" -- The Archway
    , "03329d" -- Steps of the Palace
    , "03330c" -- The Fall
    , "03331c" -- Hastur's End
    , "04117" -- Threads of Fate stuff
    , "04118"
    , -- \^^
      "04125d"
    , -- \^^
      "04126c"
    , -- \^^
      "04127c"
    , -- \^^
      "04128c"
    , -- \^^
      "04129c"
    , -- \^^
      "04130c"
    , -- \^^
      "04132c"
    , -- \^^
      "04133f"
    , -- \^^
      "04134e"
    , -- \^^
      "04134f"
    , -- \^^
      "04135e"
    , -- \^^
      "04136e"
    , -- \^^
      "04137e"
    , -- \^^
      "04138e"
    , -- \^^
      "04139e"
    , -- \^^
      "04140e"
    ]

-- \^^

runValidations :: Map CardCode CardJson -> IO ()
runValidations cards = do
  results <- getValidationResults cards
  case results of
    Right _ -> putStrLn "All good!"
    Left errs -> mapM_ (print @SomeException) errs

invariant :: (MonadValidate [SomeException] m, Exception e) => e -> m ()
invariant = dispute . pure . SomeException

getValidationResults :: Map CardCode CardJson -> IO (Either [SomeException] ())
getValidationResults cards = runValidateT $ do
  -- validate card defs
  for_ (filterTest $ mapToList allCards) $ \(ccode', card) -> do
    let ccode = normalizeCardCode ccode'
    case lookup ccode cards of
      Nothing -> unless (ignoreCardCode ccode) (invariant $ UnknownCard ccode)
      Just cardJson@CardJson {..} -> do
        if type_code == "location"
          then do
            for_ back_name $ \name' ->
              when
                (Name (normalizeName code name') Nothing /= cdName card)
                ( unless
                    (ignoreCardCode ccode)
                    ( invariant $
                        NameMismatch
                          code
                          (Name (normalizeName code name') Nothing)
                          (cdName card)
                    )
                )
            for_ (cdRevealedName card) $ \revealedName ->
              when
                ( Name (normalizeName code name) (normalizeSubname code subname)
                    /= revealedName
                )
                ( unless
                    (ignoreCardCode ccode)
                    ( invariant $
                        NameMismatch
                          code
                          ( Name
                              (normalizeName code name)
                              (normalizeSubname code subname)
                          )
                          revealedName
                    )
                )
          else do
            when
              ( Name (normalizeName code name) (normalizeSubname code subname)
                  /= cdName card
              )
              ( unless
                  (ignoreCardCode ccode)
                  ( invariant $
                      NameMismatch
                        code
                        ( Name
                            (normalizeName code name)
                            (normalizeSubname code subname)
                        )
                        (cdName card)
                  )
              )
        when
          ( isJust (cdEncounterSet card)
              && Just quantity
                /= cdEncounterSetQuantity card
              && cdCardType card
                `notElem` [ActType, AgendaType]
          )
          ( invariant $
              QuantityMismatch
                code
                (Name name subname)
                quantity
                (cdEncounterSetQuantity card)
          )
        when
          (is_unique /= cdUnique card)
          (invariant $ UniqueMismatch code (cdName card))
        when
          (fromMaybe 0 xp /= cdLevel card)
          ( invariant $ XpMismatch code (cdName card) (fromMaybe 0 xp) (cdLevel card)
          )
        when
          (normalizeCost code cost /= cdCost card)
          (invariant $ CardCostMismatch code (cdName card) cost (cdCost card))
        when
          ( sort
              ( mapMaybe
                  normalizeClassSymbol
                  (toClassSymbol faction_name : maybe [] (pure . toClassSymbol) faction2_name)
              )
              /= sort (mapMaybe (normalizeClassSymbol . Just) (setToList $ cdClassSymbols card))
          )
          ( invariant $
              ClassMismatch
                code
                (cdName card)
                faction_name
                (cdClassSymbols card)
          )
        when
          ( sort (normalizeSkills code $ getSkills cardJson)
              /= sort (cdSkills card)
          )
          ( invariant $
              SkillsMismatch
                code
                (cdName card)
                (getSkills cardJson)
                (cdSkills card)
          )
        when
          ( normalizeTraits code (getTraits cardJson)
              /= cdCardTraits card
              && normalizeTraits code (getTraits cardJson)
                /= cdRevealedCardTraits card
          )
          ( unless
              (ignoreCardCode ccode)
              ( invariant $
                  TraitsMismatch
                    code
                    (cdName card)
                    (getTraits cardJson)
                    (cdCardTraits card)
              )
          )
        when
          (victory /= cdVictoryPoints card)
          ( invariant $
              VictoryMismatch
                code
                (cdName card)
                victory
                (cdVictoryPoints card)
          )

  -- validate enemies
  for_ (filterTestEntities $ mapToList allEnemies) $
    \(ccode, SomeEnemyCard builder) -> do
      attrs <- toAttrs . cbCardBuilder builder nullCardId <$> lift getRandom
      case lookup ccode cards of
        Nothing -> unless (ignoreCardCode ccode) (invariant $ UnknownCard ccode)
        Just CardJson {..} -> do
          let
            cardStats =
              normalizeEnemyStats ccode $
                ( max 0 $ fromMaybe 0 enemy_fight
                , toGameVal
                    (fromMaybe False health_per_investigator)
                    (fromMaybe 0 health)
                , max 0 <$> enemy_evade
                )
            enemyStats =
              (enemyFight attrs, enemyHealth attrs, enemyEvade attrs)
            cardDamage =
              ( max 0 $ fromMaybe 0 enemy_damage
              , max 0 $ fromMaybe 0 enemy_horror
              )
            enemyDamage = (enemyHealthDamage attrs, enemySanityDamage attrs)

          when
            (cardStats /= enemyStats)
            ( invariant $
                EnemyStatsMismatch
                  code
                  (cdName $ toCardDef attrs)
                  cardStats
                  enemyStats
            )

          when
            (cardDamage /= enemyDamage)
            ( invariant $
                EnemyDamageMismatch
                  code
                  (cdName $ toCardDef attrs)
                  cardDamage
                  enemyDamage
            )

  for_ (Enemies.allPlayerEnemyCards <> Enemies.allEncounterEnemyCards) $
    \def -> do
      let mfunc = lookup (toCardCode def) allEnemies
      when
        (isNothing mfunc)
        (invariant $ MissingImplementation (toCardCode def) (cdName def))

  -- validate locations
  for_ (filterTestEntities $ mapToList allLocations) $
    \(ccode, SomeLocationCard builder) -> do
      attrs <- toAttrs . cbCardBuilder builder nullCardId <$> lift getRandom
      case lookup ccode cards of
        Nothing -> unless (ignoreCardCode ccode) (invariant $ UnknownCard ccode)
        Just CardJson {..} -> do
          when
            (max 0 (fromMaybe 0 shroud) /= locationShroud attrs)
            ( invariant $
                ShroudMismatch
                  code
                  (cdName $ toCardDef attrs)
                  (max 0 $ fromMaybe 0 shroud)
                  (locationShroud attrs)
            )
          when
            (fromMaybe 0 clues /= fromGameValue (locationRevealClues attrs) 1)
            ( invariant $
                ClueMismatch
                  code
                  (cdName $ toCardDef attrs)
                  (fromMaybe 0 clues)
                  (fromGameValue (locationRevealClues attrs) 1)
            )

  -- validate assets
  for_ (filterTestEntities $ mapToList allAssets) $
    \(ccode, SomeAssetCard builder) -> do
      attrs <-
        toAttrs . cbCardBuilder builder nullCardId <$> ((,Just "01001") <$> lift getRandom)
      case lookup ccode cards of
        Nothing -> unless (ignoreCardCode ccode) (invariant $ UnknownCard ccode)
        Just CardJson {..} -> do
          let
            cardStats = (health, sanity)
            assetStats = (assetHealth attrs, assetSanity attrs)
          when
            (cardStats /= assetStats)
            ( invariant $
                AssetStatsMismatch
                  code
                  (cdName $ toCardDef attrs)
                  cardStats
                  assetStats
            )

  for_ (Assets.allPlayerAssetCards <> Assets.allEncounterAssetCards) $ \def ->
    do
      let mfunc = lookup (toCardCode def) allAssets
      when
        (isNothing mfunc)
        (invariant $ MissingImplementation (toCardCode def) (cdName def))

  -- validate events
  for_ Events.allPlayerEventCards $ \def -> do
    let mfunc = lookup (toCardCode def) allEvents
    when
      (isNothing mfunc)
      (invariant $ MissingImplementation (toCardCode def) (cdName def))

  for_ Skills.allPlayerSkillCards $ \def -> do
    let mfunc = lookup (toCardCode def) allSkills
    when
      (isNothing mfunc)
      (invariant $ MissingImplementation (toCardCode def) (cdName def))

  for_
    ( Treacheries.allPlayerTreacheryCards
        <> Treacheries.allEncounterTreacheryCards
    )
    $ \def -> do
      let mfunc = lookup (toCardCode def) allTreacheries
      when
        (isNothing mfunc)
        (invariant $ MissingImplementation (toCardCode def) (cdName def))

normalizeImageCardCode :: CardCode -> Text
normalizeImageCardCode "02214" = "02214b"
normalizeImageCardCode other = unCardCode other

normalizeSkills :: CardCode -> [SkillIcon] -> [SkillIcon]
-- normalizeSkills "02230" _ = [SkillIcon SkillWillpower, SkillIcon SkillAgility]
normalizeSkills "04244" _ = [] -- Body of a Yithian
normalizeSkills "05046" _ = [] -- Gavriella Mizrah
normalizeSkills "05047" _ = [] -- Jerome Davids
normalizeSkills "05048" _ = [] -- Valentino Rivas
normalizeSkills "05049" _ = [] -- Penny White
normalizeSkills _ skills = skills

normalizeTraits :: CardCode -> Set Trait -> Set Trait
-- Erratum: Each of the Patient Confinement locations should not have the Arkham Asylum trait.
normalizeTraits "03178" _ = mempty
normalizeTraits "03179" _ = mempty
normalizeTraits "03180" _ = mempty
normalizeTraits "03181" _ = mempty
normalizeTraits _ traits = traits

runMissingImages :: IO ()
runMissingImages = do
  missing <-
    catMaybes <$> for
      (keys . filterTest $ mapToList allCards)
      \ccode -> do
        let
          filename = unpack $ normalizeImageCardCode ccode <> ".jpg"
          filepath =
            ".."
              </> "frontend"
              </> "public"
              </> "img"
              </> "arkham"
              </> "cards"
              </> filename
        exists <- doesFileExist filepath
        pure $
          if exists || ccode == "01000"
            then Nothing
            else Just (unCardCode ccode)
  traverse_ putStrLn (sort missing)

main :: IO ()
main = do
  ecards <- eitherDecodeFileStrict @[CardJson] ("data" </> "cards.json")
  let cards = either error (mapFromList . map (code &&& id)) ecards
  args <- getArgs
  case args of
    [] -> runValidations cards
    ["validate"] -> runValidations cards
    ["missing"] -> runMissing Nothing cards
    ["missing", packCode] -> runMissing (Just packCode) cards
    ["images"] -> runMissingImages
    _ -> error "Usage: [validate] | missing [pack_code]"
