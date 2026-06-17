{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Campaigns.TheFeastOfHemlockVale.Helpers where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.Campaign.Types (Field (CampaignChaosBag))
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.ChaosToken.Types (ChaosTokenFace (..), isSymbolChaosToken)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Criteria
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Types.Attrs
import Arkham.Helpers.Campaign
import Arkham.Helpers.FlavorText (chaosTokenMorph, p, setTitle, storyBuild)
import Arkham.Helpers.Investigator (getHandSize, getStartingResources)
import Arkham.Helpers.Log
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Location.Base
import Arkham.Matcher
import Arkham.Message (Message (NextCampaignStep), pattern SetCampaignChaosBag)
import Arkham.Message.Lifted hiding (continue)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (decrementRecordCount, incrementRecordCount, recordCount)
import Arkham.Modifier
import Arkham.Prelude hiding (Day)
import Arkham.Projection
import Arkham.Scenario.Import.Lifted (gather, placeStory)
import Arkham.Scenario.Options
import Arkham.Scenario.Setup (ScenarioBuilderT)
import Arkham.Source
import Arkham.Story.Cards qualified as Stories
import Arkham.Target
import Arkham.Tracing
import Arkham.Trait (Trait (Dark))
import Data.Monoid (First (..))

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theFeastOfHemlockVale" a

codex :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
codex iid (toSource -> source) n = do
  cannotTriggerCodex <- elem (ScenarioModifier "cannotTriggerCodex") <$> getModifiers ScenarioTarget
  unless cannotTriggerCodex $ scenarioSpecific "codex" (iid, source, n)

type Codex = Int

youCanTriggerCodex :: Codex -> Criterion
youCanTriggerCodex entry =
  youExist
    ( InvestigatorWithoutModifier (codexDone entry)
        <> InvestigatorWithoutModifier (ScenarioModifier "cannotTriggerCodex")
    )

codexDone :: Codex -> ModifierType
codexDone entry = ScenarioModifierValue "codex:done" (toJSON entry)

codexFinished :: ReverseQueue m => Codex -> m ()
codexFinished entry = eachInvestigator (codexFinishedFor entry)

codexFinishedUntilNewAct :: ReverseQueue m => Codex -> m ()
codexFinishedUntilNewAct entry = eachInvestigator \iid ->
  actModifier ScenarioSource iid $ codexDone entry

codexFinishedFor :: ReverseQueue m => Codex -> InvestigatorId -> m ()
codexFinishedFor entry iid = gameModifier ScenarioSource iid (ScenarioModifierValue "codex:done" (toJSON entry))

makePreparationsForNextSurvey :: ReverseQueue m => InvestigatorId -> m ()
makePreparationsForNextSurvey iid = do
  iattrs <- getAttrs @Investigator.Investigator iid
  let startingAssetCodes = map toCardCode iattrs.investigatorStartsWith
  permanentAssets <- select $ assetControlledBy iid <> PermanentAsset
  assets <- selectWithField Asset.AssetCardCode $ assetControlledBy iid
  let isPersistent (aid, code) = code `elem` startingAssetCodes || aid `elem` permanentAssets
  let (persistAssets, otherAssets) = partition isPersistent assets

  for_ persistAssets \(asset, _) -> setupModifier ScenarioSource asset Persist
  unless (null otherAssets) $ chooseOrRunOneM iid do
    for_ (eachWithRest (map fst otherAssets)) \(asset, rest) ->
      targeting asset do
        setupModifier ScenarioSource asset Persist
        for_ rest $ toDiscard ScenarioSource

  handSize <- getHandSize iid
  cardsInHand <- fieldMap Investigator.InvestigatorHand length iid
  when (cardsInHand > handSize) $ chooseAndDiscardCards iid ScenarioSource (cardsInHand - handSize)
  shuffleDiscardBackIn iid
  startingResources <- getStartingResources iid
  resources <- field Investigator.InvestigatorResources iid
  when (resources > startingResources)
    $ loseResources iid ScenarioSource (resources - startingResources)

data Day = Day1 | Day2 | Day3
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

dayNumber :: Day -> Int
dayNumber = \case
  Day1 -> 1
  Day2 -> 2
  Day3 -> 3

data Time = Night | Day
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

setupHemlockDay :: ReverseQueue m => Day -> Time -> ScenarioBuilderT m ()
setupHemlockDay day time = case day of
  Day1 -> do
    gather Set.TheFirstDay
    placeStory $ case time of
      Day -> Stories.dayOne
      Night -> Stories.nightOne
  Day2 -> do
    gather Set.TheSecondDay
    placeStory $ case time of
      Day -> Stories.dayTwo
      Night -> Stories.nightTwo
  Day3 -> do
    gather Set.TheFinalDay
    placeStory $ case time of
      Day -> Stories.dayThree
      Night -> Stories.nightThree

initMeta :: TheFeastOfHemlockValeMeta
initMeta = TheFeastOfHemlockValeMeta Day1 Day []

data TheFeastOfHemlockValeMeta = TheFeastOfHemlockValeMeta
  { day :: Day
  , time :: Time
  , chosenCodexEntries :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON TheFeastOfHemlockValeMeta where
  parseJSON = withObject "TheFeastOfHemlockValeMeta" $ \o -> do
    day <- o .: "day"
    time <- o .: "time"
    chosenCodexEntries <- o .:? "chosenCodexEntries" .!= []
    pure TheFeastOfHemlockValeMeta {..}

getCampaignTime :: (Tracing m, HasGame m) => m Time
getCampaignTime = withCampaignMeta @TheFeastOfHemlockValeMeta (.time)

getCampaignDay :: (Tracing m, HasGame m) => m Day
getCampaignDay = withCampaignMeta @TheFeastOfHemlockValeMeta (.day)

getTimeFor :: (Targetable a, Tracing m, HasGame m) => a -> m Time
getTimeFor a = do
  mods <- getModifiers a
  maybe getCampaignTime pure
    $ getFirst
    $ mconcat [First t | ScenarioModifierValue "time" (maybeResult -> Just t) <- mods]

class HasTimeOverride a where
  isDark :: a -> Criterion
  isLight :: a -> Criterion
  isLight = not_ . isDark

{- | The Lost Sister has no global day/night. Instead it is Day for everything
(locations, enemies, treacheries, investigators) unless the relevant location
has the Dark trait, in which case it is Night there. Every other scenario sets
a single global "time" modifier, so we fall back to that when not in The Lost
Sister.
-}
inLostSister :: Criterion -> Criterion -> Criterion
inLostSister = IfCriteria (ScenarioExists $ ScenarioWithId "10569")

instance HasTimeOverride EnemyAttrs where
  isDark a =
    inLostSister
      (thisExists a (EnemyWithModifier $ ScenarioModifierValue "time" (String "Night")))
      IsNight_

instance HasTimeOverride LocationAttrs where
  isDark a = inLostSister (thisExists a $ LocationWithTrait Dark) IsNight_
  isLight a = inLostSister (thisExists a $ not_ $ LocationWithTrait Dark) IsDay_

instance HasTimeOverride InvestigatorMatcher where
  isDark a =
    inLostSister
      (exists $ a <> InvestigatorWithModifier (ScenarioModifierValue "time" (String "Night")))
      IsNight_

isDayFor :: HasTimeOverride a => a -> Criterion
isDayFor a = isLight a

isNightFor :: HasTimeOverride a => a -> Criterion
isNightFor a = isDark a

pattern IsDay_ :: Criterion
pattern IsDay_ <- ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Day")))
  where
    IsDay_ = ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Day")))

pattern IsNight_ :: Criterion
pattern IsNight_ <- ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Night")))
  where
    IsNight_ = ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "time" (String "Night")))

pattern IsDay1 :: Criterion
pattern IsDay1 <- ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "day" (String "Day1")))
  where
    IsDay1 = ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "day" (String "Day1")))

pattern IsDay2 :: Criterion
pattern IsDay2 <- ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "day" (String "Day2")))
  where
    IsDay2 = ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "day" (String "Day2")))

pattern IsDay3 :: Criterion
pattern IsDay3 <- ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "day" (String "Day3")))
  where
    IsDay3 = ScenarioExists (ScenarioWithModifier (ScenarioModifierValue "day" (String "Day3")))

setScenarioDayAndTime :: ReverseQueue m => m ()
setScenarioDayAndTime = do
  day <- getCampaignDay
  time <- getCampaignTime
  gameModifier ScenarioSource ScenarioTarget (ScenarioModifierValue "day" (toJSON day))
  gameModifier ScenarioSource ScenarioTarget (ScenarioModifierValue "time" (toJSON time))

afterPrelude :: ReverseQueue m => CampaignStep -> m ()
afterPrelude =
  setNextCampaignStep . \case
    ScenarioStep sid ->
      ScenarioStepWithOptions
        sid
        defaultScenarioOptions
          { scenarioOptionsSkipInvestigatorSetup = True
          , scenarioOptionsSkipStartOfGame = True
          }
    other -> other
 where
  setNextCampaignStep = push . NextCampaignStep . continueNoUpgrade

getCrossedOutResidents :: (Tracing m, HasGame m) => m [Resident]
getCrossedOutResidents =
  catMaybes
    <$> sequence
      [ check SimeonCrossedOut SimeonAtwood
      , check LeahCrossedOut LeahAtwood
      , check TheoCrossedOut TheoPeters
      , check GideonCrossedOut GideonMizrah
      , check JudithCrossedOut JudithPark
      , check WilliamCrossedOut WilliamHemlock
      , check RiverCrossedOut RiverHawthorne
      , check MotherRachelCrossedOut MotherRachel
      ]
 where
  check k v = runMaybeT (liftGuardM (getHasRecord k) $> v)

pattern Theta :: Int
pattern Theta = 100

pattern Omega :: Int
pattern Omega = 101

pattern Psi :: Int
pattern Psi = 102

pattern Phi :: Int
pattern Phi = 103

pattern Sigma :: Int
pattern Sigma = 104

-- | Sigma sub-branches: "The Argument" outcomes from Codex Σ.
pattern SigmaJudithRight :: Int
pattern SigmaJudithRight = 105

pattern SigmaWilliamRight :: Int
pattern SigmaWilliamRight = 106

data Resident
  = WilliamHemlock
  | RiverHawthorne
  | MotherRachel
  | SimeonAtwood
  | LeahAtwood
  | TheoPeters
  | GideonMizrah
  | JudithPark
  deriving stock (Show, Eq, Bounded, Enum)

instance HasCardCode Resident where
  toCardCode = toCardCode . toCardDef

instance FetchCard Resident where
  fetchCardMaybe = fetchCardMaybe . toCardDef
  fetchCardMaybe_ = fetchCardMaybe_ . toCardDef

instance HasCardDef Resident where
  toCardDef = \case
    WilliamHemlock -> Assets.williamHemlockAspiringPoet
    RiverHawthorne -> Assets.riverHawthorneBigInNewYork
    MotherRachel -> Assets.motherRachelKindlyMatron
    SimeonAtwood -> Assets.simeonAtwoodDedicatedTroublemaker
    LeahAtwood -> Assets.leahAtwoodTheValeCook
    TheoPeters -> Assets.theoPetersJackOfAllTrades
    GideonMizrah -> Assets.gideonMizrahSeasonedSailor
    JudithPark -> Assets.judithParkTheMuscle

relationshipKey :: Resident -> CampaignLogKey
relationshipKey = \case
  WilliamHemlock -> toCampaignLogKey WilliamHemlockRelationshipLevel
  RiverHawthorne -> toCampaignLogKey RiverHawthorneRelationshipLevel
  MotherRachel -> toCampaignLogKey MotherRachelRelationshipLevel
  SimeonAtwood -> toCampaignLogKey SimeonAtwoodRelationshipLevel
  LeahAtwood -> toCampaignLogKey LeahAtwoodRelationshipLevel
  TheoPeters -> toCampaignLogKey TheoPetersRelationshipLevel
  GideonMizrah -> toCampaignLogKey GideonMizrahRelationshipLevel
  JudithPark -> toCampaignLogKey JudithParkRelationshipLevel

getRelationshipLevel :: (HasGame m, Tracing m) => Resident -> m Int
getRelationshipLevel = getRecordCount . relationshipKey

increaseRelationshipLevel :: ReverseQueue m => Resident -> Int -> m ()
increaseRelationshipLevel r = incrementRecordCount (relationshipKey r)

decreaseRelationshipLevel :: ReverseQueue m => Resident -> Int -> m ()
decreaseRelationshipLevel r = decrementRecordCount (relationshipKey r)

setRelationshipLevel :: ReverseQueue m => Resident -> Int -> m ()
setRelationshipLevel r = recordCount (relationshipKey r)

crossedOutKey :: Resident -> CampaignLogKey
crossedOutKey = \case
  WilliamHemlock -> toCampaignLogKey WilliamCrossedOut
  RiverHawthorne -> toCampaignLogKey RiverCrossedOut
  MotherRachel -> toCampaignLogKey MotherRachelCrossedOut
  SimeonAtwood -> toCampaignLogKey SimeonCrossedOut
  LeahAtwood -> toCampaignLogKey LeahCrossedOut
  TheoPeters -> toCampaignLogKey TheoCrossedOut
  GideonMizrah -> toCampaignLogKey GideonCrossedOut
  JudithPark -> toCampaignLogKey JudithCrossedOut

sacrificedKey :: Resident -> CampaignLogKey
sacrificedKey = \case
  WilliamHemlock -> toCampaignLogKey WilliamSacrificedThemselvesForTheInvestigators
  RiverHawthorne -> toCampaignLogKey RiverSacrificedThemselvesForTheInvestigators
  MotherRachel -> error "Mother Rachel cannot sacrifice herself"
  SimeonAtwood -> toCampaignLogKey SimeonSacrificedThemselvesForTheInvestigators
  LeahAtwood -> toCampaignLogKey LeahSacrificedThemselvesForTheInvestigators
  TheoPeters -> toCampaignLogKey TheoSacrificedThemselvesForTheInvestigators
  GideonMizrah -> toCampaignLogKey GideonSacrificedThemselvesForTheInvestigators
  JudithPark -> toCampaignLogKey JudithSacrificedThemselvesForTheInvestigators

residentFromCardDef :: CardDef -> Maybe Resident
residentFromCardDef def
  | def == Assets.williamHemlockAspiringPoet = Just WilliamHemlock
  | def == Assets.riverHawthorneBigInNewYork = Just RiverHawthorne
  | def == Assets.motherRachelKindlyMatron = Just MotherRachel
  | def == Assets.simeonAtwoodDedicatedTroublemaker = Just SimeonAtwood
  | def == Assets.leahAtwoodTheValeCook = Just LeahAtwood
  | def == Assets.theoPetersJackOfAllTrades = Just TheoPeters
  | def == Assets.gideonMizrahSeasonedSailor = Just GideonMizrah
  | def == Assets.judithParkTheMuscle = Just JudithPark
  | otherwise = Nothing

getAreasSurveyed :: (HasGame m, Tracing m) => m [AreasSurveyed]
getAreasSurveyed = filterM (getHasRecord . AreasSurveyed) [NorthPointMine ..]

{- | The chaos token of a value one lower, or Nothing if it cannot be lowered
(i.e. it is a symbol token or already the lowest possible value).
-}
lowerChaosTokenValue :: ChaosTokenFace -> Maybe ChaosTokenFace
lowerChaosTokenValue = \case
  PlusOne -> Just Zero
  Zero -> Just MinusOne
  MinusOne -> Just MinusTwo
  MinusTwo -> Just MinusThree
  MinusThree -> Just MinusFour
  MinusFour -> Just MinusFive
  MinusFive -> Just MinusSix
  MinusSix -> Just MinusSeven
  MinusSeven -> Just MinusEight
  _ -> Nothing

{- | The fatigue from the long night catches up to you. Draw tokens from the
chaos bag at random until you have 2 non-symbol tokens. Replace these tokens
with a chaos token of a value 1 lower for the remainder of the campaign. (If
you are unable to replace a token, repeat this process until a total of 2
chaos tokens have been replaced.)
-}
replaceFatigueChaosTokens :: ReverseQueue m => m ()
replaceFatigueChaosTokens = do
  bag <- campaignField CampaignChaosBag
  (newBag, replaced) <- go (2 :: Int) bag bag []
  unless (null replaced) $ campaignI18n $ scope "fatigue" $ storyBuild do
    -- Render each replaced token as a self-contained morph: the frontend shows
    -- the original face and then flips it in place to the lowered face. Because
    -- the whole animation lives in a single story entry (one component mount),
    -- it is immune to the game-state re-render that happens between prompts.
    setTitle "title"
    p "body"
    for_ replaced (uncurry chaosTokenMorph)
  push $ SetCampaignChaosBag newBag
 where
  -- @bag@ is the running campaign chaos bag we are mutating; @pool@ is the set
  -- of tokens we have not yet drawn this process (symbol tokens are simply
  -- returned, so we only ever draw from the non-symbol tokens left in the pool).
  -- @acc@ collects each (original, lowered) replacement, newest first.
  go 0 bag _ acc = pure (bag, reverse acc)
  go n bag pool acc = case nonEmpty (filter (not . isSymbolChaosToken) pool) of
    Nothing -> pure (bag, reverse acc)
    Just nonSymbols -> do
      face <- sample nonSymbols
      let pool' = deleteFirstMatch (== face) pool
      case lowerChaosTokenValue face of
        Just lowered -> go (n - 1) (replaceFirstMatch face lowered bag) pool' ((face, lowered) : acc)
        Nothing -> go n bag pool' acc
  replaceFirstMatch :: ChaosTokenFace -> ChaosTokenFace -> [ChaosTokenFace] -> [ChaosTokenFace]
  replaceFirstMatch _ _ [] = []
  replaceFirstMatch x x' (y : ys) = if x == y then x' : ys else y : replaceFirstMatch x x' ys
