module Main where

import ClassyPrelude

import qualified Arkham.Asset.Cards as Assets
import Arkham.EncounterCard
import qualified Arkham.Enemy.Cards as Enemies
import qualified Arkham.Event.Cards as Events
import Arkham.PlayerCard
import qualified Arkham.Skill.Cards as Skills
import qualified Arkham.Treachery.Cards as Treacheries
import Arkham.Types.Asset
import Arkham.Types.Asset.Attrs (assetHealth, assetSanity)
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.Cost
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes.Entity
import Arkham.Types.EncounterSet
import Arkham.Types.Enemy
import Arkham.Types.Enemy.Attrs
  (enemyEvade, enemyFight, enemyHealth, enemyHealthDamage, enemySanityDamage)
import Arkham.Types.Event
import Arkham.Types.GameValue
import Arkham.Types.Location
import Arkham.Types.Location.Attrs (locationRevealClues, locationShroud)
import Arkham.Types.Name
import Arkham.Types.Skill
import Arkham.Types.SkillType
import Arkham.Types.Trait hiding (Dunwich)
import Arkham.Types.Treachery
import Control.Exception
import Control.Monad.Random.Lazy
import Data.Aeson
import qualified Data.Text as T
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
  deriving anyclass FromJSON

data InternalCardCodeMismatch = InternalCardCodeMismatch CardCode CardCode
  deriving stock Show

newtype UnknownCard = UnknownCard CardCode
  deriving stock Show

data MissingImplementation = MissingImplementation CardCode Name
  deriving stock Show

data NameMismatch = NameMismatch CardCode Name Name
  deriving stock Show

data QuantityMismatch = QuantityMismatch CardCode Name Int (Maybe Int)
  deriving stock Show

data UniqueMismatch = UniqueMismatch CardCode Name
  deriving stock Show

data CardCostMismatch = CardCostMismatch
  CardCode
  Name
  (Maybe Int)
  (Maybe CardCost)
  deriving stock Show

data VictoryMismatch = VictoryMismatch CardCode Name (Maybe Int) (Maybe Int)
  deriving stock Show

data XpMismatch = XpMismatch CardCode Name Int Int
  deriving stock Show

data EnemyStatsMismatch = EnemyStatsMismatch
  CardCode
  Name
  (Int, GameValue Int, Int)
  (Int, GameValue Int, Int)
  deriving stock Show

data AssetStatsMismatch = AssetStatsMismatch
  CardCode
  Name
  (Maybe Int, Maybe Int)
  (Maybe Int, Maybe Int)
  deriving stock Show

data EnemyDamageMismatch = EnemyDamageMismatch
  CardCode
  Name
  (Int, Int)
  (Int, Int)
  deriving stock Show

data ClassMismatch = ClassMismatch CardCode Name String (Maybe ClassSymbol)
  deriving stock Show

data SkillsMismatch = SkillsMismatch CardCode Name [SkillType] [SkillType]
  deriving stock Show

data TraitsMismatch = TraitsMismatch CardCode Name (Set Trait) (Set Trait)
  deriving stock Show

data ShroudMismatch = ShroudMismatch CardCode Name Int Int
  deriving stock Show

data ClueMismatch = ClueMismatch CardCode Name Int Int
  deriving stock Show

instance Exception InternalCardCodeMismatch
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
filterTest = filter
  (\(code, cdef) -> code /= "asset" && cdEncounterSet cdef /= Just Test && not
    ("b" `isSuffixOf` unCardCode code)
  )

filterTestEntities :: [(CardCode, a)] -> [(CardCode, a)]
filterTestEntities = filter
  (\(code, _) -> code `notElem` ["enemy", "location", "asset"] && not
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
normalizeName "02219" _ = "Powder of Ibn-Ghazi"
normalizeName _ a = a

normalizeSubname :: CardCode -> Maybe Text -> Maybe Text
normalizeSubname "02230" _ = Just "... Or Are They?"
normalizeSubname _ a = a

normalizeCost :: CardCode -> Maybe Int -> Maybe CardCost
normalizeCost "02178" _ = Nothing
normalizeCost _ (Just (-2)) = Just DynamicCost
normalizeCost _ (Just n) = Just (StaticCost n)
normalizeCost _ Nothing = Nothing

allCards :: Map CardCode CardDef
allCards = allPlayerCards <> allEncounterCards

getSkills :: CardJson -> [SkillType]
getSkills CardJson {..} =
  getSkill SkillWillpower skill_willpower
    <> getSkill SkillIntellect skill_intellect
    <> getSkill SkillCombat skill_combat
    <> getSkill SkillAgility skill_agility
    <> getSkill SkillWild skill_wild
 where
  getSkill _ Nothing = []
  getSkill skillType (Just n) = replicate n skillType

getTraits :: CardJson -> Set Trait
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
      $ T.replace " " "" x
  handleEither _ (Right a) = a
  handleEither x (Left err) =
    error $ show code <> ": " <> err <> " " <> show x <> " from " <> show traits
  normalizeTrait "Human" = "Humanoid"
  normalizeTrait x = x
  cleanText = T.dropWhileEnd (\c -> c == '.' || c == ' ')

toGameVal :: Bool -> Int -> GameValue Int
toGameVal True n = PerPlayer n
toGameVal False n = Static n

runMissing :: Maybe Text -> Map CardCode CardJson -> IO ()
runMissing mPackCode cards = do
  let
    cardCodes =
      map CardCode
        . sort
        . map unCardCode
        . toList
        $ keysSet (filterOutIrrelevant mPackCode cards)
        `difference` keysSet allCards
  case cardCodes of
    [] -> putStrLn "Complete!"
    xs -> for_ (mapMaybe (`lookup` cards) xs) $ \card -> do
      let
        suffix = maybe
          ""
          (\level -> if level == 0 then "" else "(" <> tshow level <> ")")
          (xp card)
      putStrLn $ unCardCode (code card) <> ": " <> name card <> suffix

filterOutIrrelevant
  :: Maybe Text -> Map CardCode CardJson -> Map CardCode CardJson
filterOutIrrelevant mPackCode = filterMap
  (\card ->
    (code card /= "01000")
      && (type_code card `notElem` ["investigator", "scenario", "act", "agenda"]
         )
      && maybe True ((== pack_code card) . unpack) mPackCode
  )

runValidations :: Map CardCode CardJson -> IO ()
runValidations cards = do
  -- validate card defs
  for_ (filterTest $ mapToList allCards) $ \(ccode, card) -> do
    when
      (ccode /= cdCardCode card)
      (throw $ InternalCardCodeMismatch ccode (cdCardCode card))
    case lookup ccode cards of
      Nothing -> throw $ UnknownCard (cdCardCode card)
      Just cardJson@CardJson {..} -> do
        if type_code == "location"
          then do
            for_ back_name $ \name' -> when
              (Name (normalizeName code name') Nothing /= cdName card)
              (throw $ NameMismatch
                code
                (Name (normalizeName code name') Nothing)
                (cdName card)
              )
            for_ (cdRevealedName card) $ \revealedName -> when
              (Name (normalizeName code name) (normalizeSubname code subname)
              /= revealedName
              )
              (throw $ NameMismatch
                code
                (Name (normalizeName code name) (normalizeSubname code subname))
                revealedName
              )
          else do
            when
              (Name (normalizeName code name) (normalizeSubname code subname)
              /= cdName card
              )
              (throw $ NameMismatch
                code
                (Name (normalizeName code name) (normalizeSubname code subname))
                (cdName card)
              )
        when
          (isJust (cdEncounterSet card)
          && Just quantity
          /= cdEncounterSetQuantity card
          )
          (throw $ QuantityMismatch
            code
            (Name name subname)
            quantity
            (cdEncounterSetQuantity card)
          )
        when
          (is_unique /= cdUnique card)
          (throw $ UniqueMismatch code (cdName card))
        when
          (fromMaybe 0 xp /= cdLevel card)
          (throw $ XpMismatch code (cdName card) (fromMaybe 0 xp) (cdLevel card)
          )
        when
          (normalizeCost code cost /= cdCost card)
          (throw $ CardCostMismatch code (cdName card) cost (cdCost card))
        when
          (toClassSymbol faction_name /= cdClassSymbol card)
          (throw $ ClassMismatch
            code
            (cdName card)
            faction_name
            (cdClassSymbol card)
          )
        when
          (sort (normalizeSkills code $ getSkills cardJson)
          /= sort (cdSkills card)
          )
          (throw $ SkillsMismatch
            code
            (cdName card)
            (getSkills cardJson)
            (cdSkills card)
          )
        when
          (getTraits cardJson /= cdCardTraits card)
          (throw $ TraitsMismatch
            code
            (cdName card)
            (getTraits cardJson)
            (cdCardTraits card)
          )
        when
          (victory /= cdVictoryPoints card)
          (throw $ VictoryMismatch
            code
            (cdName card)
            victory
            (cdVictoryPoints card)
          )

  -- validate enemies
  for_ (filterTestEntities $ mapToList allEnemies) $ \(ccode, builder) -> do
    attrs <- toAttrs . builder <$> getRandom
    case lookup ccode cards of
      Nothing -> throw $ UnknownCard ccode
      Just CardJson {..} -> do
        let
          cardStats =
            ( max 0 $ fromMaybe 0 enemy_fight
            , toGameVal
              (fromMaybe False health_per_investigator)
              (fromMaybe 0 health)
            , max 0 $ fromMaybe 0 enemy_evade
            )
          enemyStats = (enemyFight attrs, enemyHealth attrs, enemyEvade attrs)
          cardDamage =
            (max 0 $ fromMaybe 0 enemy_damage, max 0 $ fromMaybe 0 enemy_horror)
          enemyDamage = (enemyHealthDamage attrs, enemySanityDamage attrs)
        when
          (cardStats /= enemyStats)
          (throw $ EnemyStatsMismatch
            code
            (cdName $ toCardDef attrs)
            cardStats
            enemyStats
          )
        when
          (cardDamage /= enemyDamage)
          (throw $ EnemyDamageMismatch
            code
            (cdName $ toCardDef attrs)
            cardDamage
            enemyDamage
          )

  for_ (Enemies.allPlayerEnemyCards <> Enemies.allEncounterEnemyCards)
    $ \def -> do
        let mfunc = lookup (toCardCode def) allEnemies
        when
          (isNothing mfunc)
          (throw $ MissingImplementation (toCardCode def) (cdName def))

  -- validate locations
  for_ (filterTestEntities $ mapToList allLocations) $ \(ccode, builder) -> do
    attrs <- toAttrs . builder <$> getRandom
    case lookup ccode cards of
      Nothing -> throw $ UnknownCard ccode
      Just CardJson {..} -> do
        when
          (fromMaybe 0 shroud /= locationShroud attrs)
          (throw $ ShroudMismatch
            code
            (cdName $ toCardDef attrs)
            (fromMaybe 0 shroud)
            (locationShroud attrs)
          )
        when
          (fromMaybe 0 clues /= fromGameValue (locationRevealClues attrs) 1)
          (throw $ ClueMismatch
            code
            (cdName $ toCardDef attrs)
            (fromMaybe 0 clues)
            (fromGameValue (locationRevealClues attrs) 1)
          )

  -- validate assets
  for_ (filterTestEntities $ mapToList allAssets) $ \(ccode, builder) -> do
    attrs <- toAttrs . builder <$> getRandom
    case lookup ccode cards of
      Nothing -> throw $ UnknownCard ccode
      Just CardJson {..} -> do
        let
          cardStats = (health, sanity)
          assetStats = (assetHealth attrs, assetSanity attrs)
        when
          (cardStats /= assetStats)
          (throw $ AssetStatsMismatch
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
        (throw $ MissingImplementation (toCardCode def) (cdName def))

  -- validate events
  for_ Events.allPlayerEventCards $ \def -> do
    let mfunc = lookup (toCardCode def) allEvents
    when
      (isNothing mfunc)
      (throw $ MissingImplementation (toCardCode def) (cdName def))

  for_ Skills.allPlayerSkillCards $ \def -> do
    let mfunc = lookup (toCardCode def) allSkills
    when
      (isNothing mfunc)
      (throw $ MissingImplementation (toCardCode def) (cdName def))

  for_
      (Treacheries.allPlayerTreacheryCards
      <> Treacheries.allEncounterTreacheryCards
      )
    $ \def -> do
        let mfunc = lookup (toCardCode def) allTreacheries
        when
          (isNothing mfunc)
          (throw $ MissingImplementation (toCardCode def) (cdName def))

normalizeImageCardCode :: CardCode -> Text
normalizeImageCardCode "02214" = "02214b"
normalizeImageCardCode other = unCardCode other

normalizeSkills :: CardCode -> [SkillType] -> [SkillType]
normalizeSkills "02230" _ = [SkillWillpower, SkillAgility]
normalizeSkills _ skills = skills

runMissingImages :: IO ()
runMissingImages = do
  missing <- catMaybes <$> for
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
      pure $ if exists then Nothing else Just (unCardCode ccode)
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
