{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Main where

import ClassyPrelude hiding (throwIO)

import Arkham.Asset
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (SomeAssetCard (..), assetHealth, assetSanity)
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes.Entity
import Arkham.EncounterCard
import Arkham.EncounterSet ()
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
import Arkham.Investigator.Cards
import Arkham.Location
import Arkham.Location.Types (
  SomeLocationCard (..),
  locationRevealClues,
  locationShroud,
 )
import Arkham.Name
import Arkham.PlayerCard
import Arkham.Scenario
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
import GHC.Stack
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

data UnknownCard = UnknownCard CardCode [(String, SrcLoc)]
  deriving stock (Show)

unknownCard :: HasCallStack => CardCode -> UnknownCard
unknownCard code = UnknownCard code (getCallStack callStack)

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

data XpMismatch = XpMismatch CardCode Name (Maybe Int) (Maybe Int)
  deriving stock (Show)

data EnemyStatsMismatch
  = EnemyStatsMismatch
      CardCode
      Name
      (Maybe Int, Maybe GameValue, Maybe Int)
      (Maybe Int, Maybe GameValue, Maybe Int)
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
normalizeName "02159" _ = "The Essex County Express" -- missing The
normalizeName "02219" _ = "Powder of Ibn-Ghazi" -- missing -
normalizeName "06078" _ = "The Infestation Begins" -- includes a ... not in the name
normalizeName "84025" _ = "Blood on Your Hands" -- your is lowercase on ADB
normalizeName _ a = T.replace "\8217" "'" a

normalizeSubname :: CardCode -> Maybe Text -> Maybe Text
normalizeSubname _ a = T.replace "\8217" "'" <$> a

normalizeCost :: CardCode -> Maybe Int -> Maybe CardCost
normalizeCost "02178" _ = Nothing -- has Just 0 for a treachery
normalizeCost "06034" (Just (-2)) = Just DiscardAmountCost
normalizeCost _ (Just (-2)) = Just DynamicCost
normalizeCost _ (Just n) = Just (StaticCost n)
normalizeCost _ Nothing = Nothing

allCards :: Map CardCode CardDef
allCards =
  allInvestigatorCards
    <> allPlayerCards
    <> allEncounterCards
    <> allScenarioCards
    <> allEncounterInvestigatorCards

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
  Just "" -> mempty -- Cards with removed traits like Patient Confinement: Daniel's Cell can show up this way
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
      $ T.replace "'" ""
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

-- Convert our card code into the card code on ADB
normalizeCardCode :: CardCode -> CardCode
normalizeCardCode (CardCode c) =
  if
    | c `elem` maskedCarnevaleGoers -> "82017b"
    | c `elem` cardsThatShouldNotHaveSuffix ->
        CardCode $ take (length c - 1) c
    | otherwise -> CardCode c
 where
  -- Masked Carnevale-Goer is treated as 1 card on adb
  maskedCarnevaleGoers = ["82017b", "82018b", "82019b", "82020b", "82021b"]
  -- Sometimes arkhamdb will give an a suffix to a card that doesn't need it
  -- Sometimes arkhamdb will not use the suffix even if printed on the card
  cardsThatShouldNotHaveSuffix = ["03221a", "05217a"]

allCardCodes :: Set CardCode
allCardCodes = setFromList $ concatMap (\c -> cdCardCode c : cdAlternateCardCodes c) allCards

runMissing :: Maybe Text -> Map CardCode CardJson -> IO ()
runMissing mPackCode cards = do
  let
    cardCodes =
      map CardCode
        . sort
        . map (unCardCode . normalizeCardCode)
        . toList
        $ keysSet (filterOutIrrelevant mPackCode cards)
          `difference` allCardCodes
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
filterOutIrrelevant _mPackCode = id

-- filterMap
--   ( \card ->
--       (code card /= "01000")
--         && ( type_code card `notElem` ["investigator", "scenario", "act", "agenda"]
--            )
--         && maybe True ((== pack_code card) . unpack) mPackCode
--   )

normalizeClassSymbol :: Maybe ClassSymbol -> Maybe ClassSymbol
normalizeClassSymbol (Just Mythos) = Nothing
normalizeClassSymbol c = c

normalizeEnemyStats
  :: CardCode -> (Maybe Int, Maybe GameValue, Maybe Int) -> (Maybe Int, Maybe GameValue, Maybe Int)
normalizeEnemyStats "05085" (fight, _, evade) = (fight, Just (Static 3), evade) -- ADB incorrectly has this as 2 health
normalizeEnemyStats _ stats = stats

normalizeEnemyDamage :: CardCode -> (Int, Int) -> (Int, Int)
normalizeEnemyDamage "03067b" _ = (1, 1) -- ADB incorrectly has this as 4 damage
normalizeEnemyDamage "05263b" _ = (1, 1) -- ADB incorrectly has this as no horror
normalizeEnemyDamage "05264b" _ = (0, 2) -- ADB incorrectly has this as no horror
normalizeEnemyDamage "05265b" _ = (1, 1) -- ADB incorrectly has this as no horror
normalizeEnemyDamage _ damage = damage

ignoreCardCode :: CardCode -> Bool
ignoreCardCode x = T.isPrefixOf "x" (unCardCode x) || x `elem` ignoredCardCodes
 where
  ignoredCardCodes = []

runValidations :: Map CardCode CardJson -> IO ()
runValidations cards = do
  results <- getValidationResults cards
  case results of
    Right _ -> putStrLn "All good!"
    Left errs -> mapM_ (print @SomeException) errs

invariant :: (MonadValidate [SomeException] m, Exception e) => e -> m ()
invariant = dispute . pure . SomeException

invariantWhen :: (MonadValidate [SomeException] m, Exception e) => Bool -> e -> m ()
invariantWhen invariantPred e = when invariantPred (invariant e)

getValidationResults :: Map CardCode CardJson -> IO (Either [SomeException] ())
getValidationResults cards = runValidateT $ do
  -- validate card defs
  for_ (mapToList allCards) $ \(ccode', card) -> do
    let ccode = normalizeCardCode ccode'
    case lookup ccode cards of
      Nothing -> unless (ignoreCardCode ccode) (invariant $ unknownCard ccode)
      Just cardJson@CardJson {..} -> do
        if type_code == "location"
          then do
            for_ back_name $ \name' ->
              when (Name (normalizeName code name') Nothing /= cdName card) do
                unless (ignoreCardCode ccode) do
                  invariant $ NameMismatch code (Name (normalizeName code name') Nothing) (cdName card)
            for_ (cdRevealedName card) $ \revealedName ->
              when (Name (normalizeName code name) (normalizeSubname code subname) /= revealedName) do
                unless (ignoreCardCode ccode) do
                  invariant $
                    NameMismatch code (Name (normalizeName code name) (normalizeSubname code subname)) revealedName
          else do
            when (Name (normalizeName code name) (normalizeSubname code subname) /= cdName card) do
              unless (ignoreCardCode ccode) do
                invariant $
                  NameMismatch code (Name (normalizeName code name) (normalizeSubname code subname)) (cdName card)

        -- Masked Carnevale-Goer is split up differently so we say the quantity is 1
        -- ideally we'd add them all up
        let quantity' = if code == "82017b" then 1 else quantity

        invariantWhen
          ( isJust (cdEncounterSet card)
              && Just quantity'
                /= cdEncounterSetQuantity card
              && cdCardType card
                `notElem` [ActType, AgendaType]
          )
          $ QuantityMismatch
            code
            (Name name subname)
            quantity
            (cdEncounterSetQuantity card)

        let isUnique = if code == "06329" then True else is_unique -- Shining Trapezohedron is unique
        when
          (isUnique /= cdUnique card)
          (invariant $ UniqueMismatch code (cdName card))
        when
          (xp /= cdLevel card)
          ( invariant $ XpMismatch code (cdName card) xp (cdLevel card)
          )
        when
          (normalizeCost code cost /= cdCost card)
          (invariant $ CardCostMismatch code (cdName card) cost (cdCost card))

        -- story types copy their front, so we ignore them
        when
          ( sort
              ( mapMaybe
                  normalizeClassSymbol
                  (toClassSymbol faction_name : maybe [] (pure . toClassSymbol) faction2_name)
              )
              /= sort (mapMaybe (normalizeClassSymbol . Just) (setToList $ cdClassSymbols card))
              && type_code /= "story"
          )
          $ do
            invariant $ ClassMismatch code (cdName card) faction_name (cdClassSymbols card)
        when
          ( sort (normalizeSkills code $ getSkills cardJson)
              /= sort (replaceWildMinus $ cdSkills card)
              && cdCardType card /= InvestigatorType
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
  for_ (mapToList allEnemies) $
    \(ccode', SomeEnemyCard builder) -> do
      let ccode = normalizeCardCode ccode'
      attrs <- toAttrs . cbCardBuilder builder nullCardId <$> lift getRandom
      case lookup ccode cards of
        Nothing -> unless (ignoreCardCode ccode) (invariant $ unknownCard ccode)
        Just CardJson {..} -> do
          let
            cardStats =
              normalizeEnemyStats ccode $
                ( max 0 <$> enemy_fight
                , toGameVal (fromMaybe False health_per_investigator) <$> health
                , max 0 <$> enemy_evade
                )
            enemyStats = (enemyFight attrs, enemyHealth attrs, enemyEvade attrs)
            cardDamage =
              normalizeEnemyDamage ccode $
                ( max 0 $ fromMaybe 0 enemy_damage
                , max 0 $ fromMaybe 0 enemy_horror
                )
            enemyDamage = (enemyHealthDamage attrs, enemySanityDamage attrs)

          invariantWhen (cardStats /= enemyStats) $
            EnemyStatsMismatch ccode (cdName $ toCardDef attrs) cardStats enemyStats

          invariantWhen (cardDamage /= enemyDamage) $
            EnemyDamageMismatch ccode (cdName $ toCardDef attrs) cardDamage enemyDamage

  for_ (Enemies.allPlayerEnemyCards <> Enemies.allEncounterEnemyCards) $
    \def -> do
      let mfunc = lookup (toCardCode def) allEnemies
      when
        (isNothing mfunc)
        (invariant $ MissingImplementation (toCardCode def) (cdName def))

  -- validate locations
  for_ (mapToList allLocations) $
    \(ccode, SomeLocationCard builder) -> do
      attrs <- toAttrs . cbCardBuilder builder nullCardId <$> lift getRandom
      case lookup ccode cards of
        Nothing -> unless (ignoreCardCode ccode) (invariant $ unknownCard ccode)
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
  for_ (mapToList allAssets) $
    \(ccode', SomeAssetCard builder) -> do
      attrs <-
        toAttrs . cbCardBuilder builder nullCardId <$> ((,Just "01001") <$> lift getRandom)
      let ccode = normalizeCardCode ccode'
      case lookup ccode cards of
        Nothing -> unless (ignoreCardCode ccode) (invariant $ unknownCard ccode)
        Just CardJson {..} -> do
          let
            cardStats = case (health, sanity) of
              (Just h, Just s) -> (guard (h > 0) $> h, guard (s > 0) $> s)
              (Just h, Nothing) -> (guard (h > 0) $> h, Nothing)
              (Nothing, Just s) -> (Nothing, guard (s > 0) $> s)
              (Nothing, Nothing) -> (Nothing, Nothing)
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
  for_ (Events.allPlayerEventCards <> Events.allEncounterEventCards) $ \def -> do
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
-- normalizeImageCardCode "02214" = "02214b"
normalizeImageCardCode other = unCardCode other

replaceWildMinus :: [SkillIcon] -> [SkillIcon]
replaceWildMinus = map $ \case
  WildMinusIcon -> WildIcon
  other -> other

normalizeSkills :: CardCode -> [SkillIcon] -> [SkillIcon]
normalizeSkills "04244" _ = [] -- Body of a Yithian, investigator stats listed as skills
normalizeSkills "05046" _ = [] -- Gavriella Mizrah, investigator stats listed as
normalizeSkills "05047" _ = [] -- Jerome Davids, investigator stats listed as
normalizeSkills "05048" _ = [] -- Valentino Rivas, investigator stats listed as
normalizeSkills "05049" _ = [] -- Penny White, investigator stats listed as
normalizeSkills _ skills = skills

normalizeTraits :: CardCode -> Set Trait -> Set Trait
-- Erratum: Each of the Patient Confinement locations should not have the Arkham Asylum trait.
-- normalizeTraits "03178" _ = mempty
-- normalizeTraits "03179" _ = mempty
-- normalizeTraits "03180" _ = mempty
-- normalizeTraits "03181" _ = mempty
normalizeTraits _ traits = traits

runMissingImages :: IO ()
runMissingImages = do
  missing <-
    catMaybes <$> for
      (keys $ mapToList allCards)
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
