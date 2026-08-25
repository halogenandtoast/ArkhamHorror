{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}

module Arkham.Campaign.Types where

import Arkham.Ability.Types (Ability)
import Arkham.Ability.Used
import Arkham.CampaignLog
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.ClassSymbol
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Decklist.RandomBasicWeakness
import Arkham.Decklist.Type
import Arkham.Difficulty
import Arkham.Helpers
import Arkham.I18n
import Arkham.Id
import Arkham.Json
import Arkham.Modifier
import Arkham.PlayerCard
import Arkham.Prelude
import Arkham.Projection
import Arkham.Resolution
import Arkham.Source
import Arkham.Target
import Arkham.Tarot
import Arkham.Xp
import Control.Monad.Writer
import Data.Aeson.TH
import Data.Aeson.Types (Parser)
import Data.Data
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import GHC.Records

class
  ( Typeable a
  , Show a
  , Eq a
  , ToJSON a
  , FromJSON a
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ CampaignId
  , EntityAttrs a ~ CampaignAttrs
  , RunType a ~ a
  ) =>
  IsCampaign a
  where
  nextStep :: a -> Maybe CampaignStep
  invalidCards :: a -> [CardCode]
  invalidCards _ = []
  campaignTokens :: Difficulty -> [ChaosTokenFace]
  campaignAbilities :: a -> [Ability]
  campaignAbilities _ = []

data instance Field Campaign :: Type -> Type where
  CampaignCompletedSteps :: Field Campaign [CampaignStep]
  CampaignStoryCards :: Field Campaign (Map InvestigatorId [Card])
  CampaignCampaignLog :: Field Campaign CampaignLog
  CampaignChaosBag :: Field Campaign [ChaosTokenFace]
  CampaignDecks :: Field Campaign (Map InvestigatorId (Deck PlayerCard))
  CampaignMeta :: Field Campaign Value
  CampaignStore :: Field Campaign (Map Text Value)
  CampaignInvalidCards :: Field Campaign [CardCode]
  CampaignDestiny :: Field Campaign (Map Scope TarotCard)
  CampaignUsedAbilities :: Field Campaign [UsedAbility]

data XpBreakdownStep = XpBreakdownStep
  { xbsStep :: CampaignStep
  , xbsInvestigators :: [InvestigatorId]
  , xbsEntries :: XpBreakdown
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

{- | A recorded change to the campaign chaos bag, grouped by the campaign step it
happened during. The bag is kept in full on both sides so the campaign log can show
what was added and removed (the multiset difference) and what the bag looked like
before and after.
-}
data ChaosBagChange = ChaosBagChange
  { cbcStep :: CampaignStep
  , cbcBefore :: [ChaosTokenFace]
  , cbcAfter :: [ChaosTokenFace]
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

data CampaignAttrs = CampaignAttrs
  { campaignId :: CampaignId
  , campaignName :: Text
  , campaignDecks :: Map InvestigatorId (Deck PlayerCard)
  , campaignStoryCards :: Map InvestigatorId [Card]
  , campaignDifficulty :: Difficulty
  , campaignChaosBag :: [ChaosTokenFace]
  , campaignChaosBagHistory :: [ChaosBagChange]
  , campaignLog :: CampaignLog
  , campaignStep :: CampaignStep
  , campaignCompletedSteps :: [CampaignStep]
  , campaignResolutions :: Map ScenarioId Resolution
  , campaignXpBreakdown :: [XpBreakdownStep]
  , campaignModifiers :: Map InvestigatorId [Modifier]
  , -- | Modifiers that apply to /all/ investigators for the remainder of the
    -- campaign (current and future). Unlike 'campaignModifiers' these are not
    -- snapshotted per-investigator; they are expanded onto every investigator
    -- when modifiers are collected.
    campaignModifiersForAll :: [ModifierType]
  , campaignMeta :: Value
  , campaignStore :: Map Text Value
  , campaignDestiny :: Map Scope TarotCard
  , campaignUsedAbilities :: [UsedAbility]
  }
  deriving stock (Show, Eq, Generic)

instance HasField "id" CampaignAttrs CampaignId where
  getField = campaignId

instance HasField "step" CampaignAttrs CampaignStep where
  getField = campaignStep

instance HasField "normalizedStep" CampaignAttrs CampaignStep where
  getField = (.normalize) . campaignStep

instance HasField "resolutions" CampaignAttrs (Map ScenarioId Resolution) where
  getField = campaignResolutions

instance HasField "chaosBag" CampaignAttrs [ChaosTokenFace] where
  getField = campaignChaosBag

instance HasField "completedSteps" CampaignAttrs [CampaignStep] where
  getField = campaignCompletedSteps

instance HasField "decks" CampaignAttrs (Map InvestigatorId (Deck PlayerCard)) where
  getField = campaignDecks

instance HasField "storyCards" CampaignAttrs (Map InvestigatorId [Card]) where
  getField = campaignStoryCards

instance HasField "log" CampaignAttrs CampaignLog where
  getField = campaignLog

instance HasField "difficulty" CampaignAttrs Difficulty where
  getField = campaignDifficulty

instance HasField "meta" CampaignAttrs Value where
  getField = campaignMeta

instance HasField "store" CampaignAttrs (Map Text Value) where
  getField = campaignStore

instance HasField "id" Campaign CampaignId where
  getField = (.id) . toAttrs

instance HasField "step" Campaign CampaignStep where
  getField = (.step) . toAttrs

instance HasField "scenario" Campaign (Maybe ScenarioId) where
  getField = (.scenario) . (.step) . toAttrs

instance HasField "normalizedStep" Campaign CampaignStep where
  getField = (.normalize) . (.step)

instance HasField "resolutions" Campaign (Map ScenarioId Resolution) where
  getField = (.resolutions) . toAttrs

instance HasField "completedSteps" Campaign [CampaignStep] where
  getField = (.completedSteps) . toAttrs

instance HasField "decks" Campaign (Map InvestigatorId (Deck PlayerCard)) where
  getField = (.decks) . toAttrs

instance HasField "difficulty" Campaign Difficulty where
  getField = (.difficulty) . toAttrs

instance HasField "meta" Campaign Value where
  getField = (.meta) . toAttrs

instance HasModifiersFor CampaignAttrs where
  getModifiersFor attrs = tell $ MonoidalMap $ Map.mapKeys toTarget $ campaignModifiers attrs

instance Sourceable CampaignAttrs where
  toSource _ = CampaignSource

completedStepsL :: Lens' CampaignAttrs [CampaignStep]
completedStepsL =
  lens campaignCompletedSteps $ \m x -> m {campaignCompletedSteps = x}

chaosBagL :: Lens' CampaignAttrs [ChaosTokenFace]
chaosBagL = lens campaignChaosBag $ \m x -> m {campaignChaosBag = x}

storyCardsL :: Lens' CampaignAttrs (Map InvestigatorId [Card])
storyCardsL = lens campaignStoryCards $ \m x -> m {campaignStoryCards = x}

decksL :: Lens' CampaignAttrs (Map InvestigatorId (Deck PlayerCard))
decksL = lens campaignDecks $ \m x -> m {campaignDecks = x}

logL :: Lens' CampaignAttrs CampaignLog
logL = lens campaignLog $ \m x -> m {campaignLog = x}

stepL :: Lens' CampaignAttrs CampaignStep
stepL = lens campaignStep $ \m x -> m {campaignStep = x}

metaL :: Lens' CampaignAttrs Value
metaL = lens campaignMeta $ \m x -> m {campaignMeta = x}

storeL :: Lens' CampaignAttrs (Map Text Value)
storeL = lens campaignStore $ \m x -> m {campaignStore = x}

destinyL :: Lens' CampaignAttrs (Map Scope TarotCard)
destinyL = lens campaignDestiny $ \m x -> m {campaignDestiny = x}

resolutionsL :: Lens' CampaignAttrs (Map ScenarioId Resolution)
resolutionsL = lens campaignResolutions $ \m x -> m {campaignResolutions = x}

usedAbilitiesL :: Lens' CampaignAttrs [UsedAbility]
usedAbilitiesL = lens campaignUsedAbilities $ \m x -> m {campaignUsedAbilities = x}

xpBreakdownL :: Lens' CampaignAttrs [XpBreakdownStep]
xpBreakdownL = lens campaignXpBreakdown $ \m x -> m {campaignXpBreakdown = x}

chaosBagHistoryL :: Lens' CampaignAttrs [ChaosBagChange]
chaosBagHistoryL = lens campaignChaosBagHistory $ \m x -> m {campaignChaosBagHistory = x}

{- | Change the campaign chaos bag, recording the change so the campaign log can show
the bag's history. Everything that happens during one campaign step folds into a
single entry, and an entry that nets back to where it started is dropped.
-}
overCampaignChaosBag :: ([ChaosTokenFace] -> [ChaosTokenFace]) -> CampaignAttrs -> CampaignAttrs
overCampaignChaosBag f attrs
  | sort before == sort after = attrs
  | otherwise = attrs & chaosBagL .~ after & chaosBagHistoryL %~ record
 where
  before = campaignChaosBag attrs
  after = f before
  step = normalizedCampaignStep (campaignStep attrs)
  record = \case
    c : rest | c.cbcStep == step -> [c {cbcAfter = after} | sort c.cbcBefore /= sort after] <> rest
    history -> ChaosBagChange step before after : history

completeStep :: CampaignStep -> [CampaignStep] -> [CampaignStep]
completeStep step' steps = step' : steps

modifiersL :: Lens' CampaignAttrs (Map InvestigatorId [Modifier])
modifiersL = lens campaignModifiers $ \m x -> m {campaignModifiers = x}

modifiersForAllL :: Lens' CampaignAttrs [ModifierType]
modifiersForAllL = lens campaignModifiersForAll $ \m x -> m {campaignModifiersForAll = x}

instance Entity CampaignAttrs where
  type EntityId CampaignAttrs = CampaignId
  type EntityAttrs CampaignAttrs = CampaignAttrs
  toId = campaignId
  toAttrs = id
  overAttrs f = f

getRandomBasicWeakness :: MonadRandom m => ClassSymbol -> Int -> Maybe ArkhamDBDecklist -> m CardDef
getRandomBasicWeakness = getRandomBasicWeaknessExcluding []

-- | 'getRandomBasicWeakness', skipping weaknesses whose canonical card code is excluded.
getRandomBasicWeaknessExcluding
  :: MonadRandom m => [CardCode] -> ClassSymbol -> Int -> Maybe ArkhamDBDecklist -> m CardDef
getRandomBasicWeaknessExcluding excluded investigatorClass playerCount mDecklist =
  sampleRandomBasicWeaknessExcluding
    excluded
    RandomBasicWeaknessContext
      { rbwInvestigatorClass = investigatorClass
      , rbwPlayerCount = playerCount
      , rbwDecklist = mDecklist
      , rbwStandalone = False
      }

{- | Replace every random basic weakness placeholder in the deck with an actual weakness.
Each draw excludes what the earlier draws produced, and the basic weaknesses already in
the deck, since only one physical copy of each exists (#5424).
-}
addRandomBasicWeaknessIfNeeded
  :: CardGen m
  => ClassSymbol -> Int -> Maybe ArkhamDBDecklist -> Deck PlayerCard -> m (Deck PlayerCard, [Card])
addRandomBasicWeaknessIfNeeded investigatorClass playerCount mDecklist deck = do
  let (placeholders, rest) = partition ((== randomWeakness) . toCardDef) (unDeck deck)
  weaknesses <- foldM drawWeakness [] placeholders
  pure (Deck rest, weaknesses)
 where
  inDeck = basicWeaknessCodes $ unDeck deck
  drawWeakness acc _ = do
    cardDef <-
      getRandomBasicWeaknessExcluding
        (inDeck <> basicWeaknessCodes acc)
        investigatorClass
        playerCount
        mDecklist
    card <- genCard cardDef
    pure $ acc <> [card]

-- | The canonical card codes of the basic weaknesses among these cards.
basicWeaknessCodes :: HasCardDef a => [a] -> [CardCode]
basicWeaknessCodes =
  map canonicalCardCode . filter ((== Just BasicWeakness) . cdCardSubType) . map toCardDef

campaignWith
  :: forall a
   . IsCampaign a
  => (CampaignAttrs -> a)
  -> CampaignId
  -> Text
  -> (CampaignAttrs -> CampaignAttrs)
  -> Difficulty
  -> a
campaignWith f campaignId' name g difficulty = campaign (f . g) campaignId' name difficulty

campaign
  :: forall a
   . IsCampaign a
  => (CampaignAttrs -> a)
  -> CampaignId
  -> Text
  -> Difficulty
  -> a
campaign f campaignId' name difficulty =
  f
    $ CampaignAttrs
      { campaignId = campaignId'
      , campaignName = name
      , campaignDecks = mempty
      , campaignStoryCards = mempty
      , campaignDifficulty = difficulty
      , campaignChaosBag = campaignTokens @a difficulty
      , campaignChaosBagHistory = mempty
      , campaignLog = mkCampaignLog
      , campaignStep = ContinueCampaignStep $ Continuation PrologueStep False False Nothing False
      , campaignCompletedSteps = []
      , campaignResolutions = mempty
      , campaignModifiers = mempty
      , campaignModifiersForAll = mempty
      , campaignMeta = Null
      , campaignStore = mempty
      , campaignXpBreakdown = mempty
      , campaignDestiny = mempty
      , campaignUsedAbilities = mempty
      }

instance Entity Campaign where
  type EntityId Campaign = CampaignId
  type EntityAttrs Campaign = CampaignAttrs
  toId = toId . toAttrs
  toAttrs (Campaign a) = toAttrs a
  overAttrs f (Campaign a) = Campaign $ overAttrs f a

instance Targetable Campaign where
  toTarget _ = CampaignTarget

data Campaign = forall a. IsCampaign a => Campaign a

instance Data Campaign where
  gunfold _ _ _ = error "gunfold(Campaign)"
  toConstr _ = error "toConstr(Campaign)"
  dataTypeOf _ = error "dataTypeOf(Campaign)"

instance HasAbilities Campaign where
  getAbilities (Campaign a) = campaignAbilities a

instance Eq Campaign where
  (Campaign (a :: a)) == (Campaign (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Campaign where
  show (Campaign a) = show a

instance ToJSON Campaign where
  toJSON (Campaign a) = toJSON a

instance HasModifiersFor Campaign where
  getModifiersFor (Campaign a) = getModifiersFor a

difficultyOf :: Campaign -> Difficulty
difficultyOf = campaignDifficulty . toAttrs

chaosBagOf :: Campaign -> [ChaosTokenFace]
chaosBagOf = campaignChaosBag . toAttrs

$(deriveToJSON (aesonOptions $ Just "campaign") ''CampaignAttrs)

instance ToJSON XpBreakdownStep where
  toJSON xs = object
    [ "step" .= xs.xbsStep
    , "investigators" .= xs.xbsInvestigators
    , "entries" .= xs.xbsEntries
    ]

instance FromJSON XpBreakdownStep where
  parseJSON = withObject "XpBreakdownStep" $ \o ->
    XpBreakdownStep <$> o .: "step" <*> o .: "investigators" <*> o .: "entries"

instance ToJSON ChaosBagChange where
  toJSON c = object ["step" .= c.cbcStep, "before" .= c.cbcBefore, "after" .= c.cbcAfter]

instance FromJSON ChaosBagChange where
  parseJSON = withObject "ChaosBagChange" $ \o ->
    ChaosBagChange <$> o .: "step" <*> o .: "before" <*> o .: "after"

oldBreakdown :: Map ScenarioId XpBreakdown -> [(CampaignStep, XpBreakdown)]
oldBreakdown = map (first ScenarioStep) . Map.toList

instance FromJSON CampaignAttrs where
  parseJSON = withObject "CampaignAttrs" $ \o -> do
    campaignId <- o .: "id"
    campaignName <- o .: "name"
    campaignDecks <- o .: "decks"
    let
      parseEitherCards :: Map InvestigatorId [Value] -> Parser (Map InvestigatorId [Card])
      parseEitherCards = traverse (traverse parseEitherCard)

      parseEitherCard :: Value -> Parser Card
      parseEitherCard v =
        parseJSON v
          <|> (toCard @PlayerCard <$> parseJSON v)
    campaignStoryCards :: Map InvestigatorId [Card] <-
      (o .: "storyCards") <|> (o .: "storyCards" >>= parseEitherCards)
    campaignDifficulty <- o .: "difficulty"
    campaignChaosBag <- o .: "chaosBag"
    campaignChaosBagHistory <- o .:? "chaosBagHistory" .!= mempty
    campaignLog <- o .: "log"
    campaignStep <- o .: "step"
    campaignCompletedSteps <- o .: "completedSteps"
    campaignResolutions <- o .: "resolutions"
    let deckIids = Map.keys campaignDecks
        toXpBreakdownStep (s, e) = XpBreakdownStep s deckIids e
    campaignXpBreakdown <-
      (map toXpBreakdownStep . oldBreakdown <$> o .: "xpBreakdown")
      <|> (map toXpBreakdownStep <$> o .:? "xpBreakdown" .!= mempty)
      <|> (o .:? "xpBreakdown" .!= mempty)
    campaignModifiers <- o .: "modifiers"
    campaignModifiersForAll <- o .:? "modifiersForAll" .!= mempty
    campaignMeta <- o .: "meta"
    campaignStore <- o .:? "store" .!= mempty
    campaignDestiny <- o .:? "destiny" .!= mempty
    campaignUsedAbilities <- o .:? "usedAbilities" .!= mempty

    pure CampaignAttrs {..}
