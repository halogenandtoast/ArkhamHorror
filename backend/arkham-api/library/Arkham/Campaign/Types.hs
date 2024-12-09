{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}

module Arkham.Campaign.Types where

import Arkham.Prelude

import Arkham.CampaignLog
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.ClassSymbol
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Difficulty
import Arkham.Helpers
import Arkham.Id
import Arkham.Json
import Arkham.Modifier
import Arkham.PlayerCard
import Arkham.Projection
import Arkham.Resolution
import Arkham.Source
import Arkham.Target
import Arkham.Xp
import Control.Monad.Writer hiding (filterM)
import Data.Aeson.TH
import Data.List.NonEmpty qualified as NE
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import Data.Typeable
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
  ) =>
  IsCampaign a
  where
  nextStep :: a -> Maybe CampaignStep

data instance Field Campaign :: Type -> Type where
  CampaignCompletedSteps :: Field Campaign [CampaignStep]
  CampaignStoryCards :: Field Campaign (Map InvestigatorId [PlayerCard])
  CampaignCampaignLog :: Field Campaign CampaignLog
  CampaignDecks :: Field Campaign (Map InvestigatorId (Deck PlayerCard))
  CampaignMeta :: Field Campaign Value
  CampaignStore :: Field Campaign (Map Text Value)

data CampaignAttrs = CampaignAttrs
  { campaignId :: CampaignId
  , campaignName :: Text
  , campaignDecks :: Map InvestigatorId (Deck PlayerCard)
  , campaignStoryCards :: Map InvestigatorId [PlayerCard]
  , campaignDifficulty :: Difficulty
  , campaignChaosBag :: [ChaosTokenFace]
  , campaignLog :: CampaignLog
  , campaignStep :: CampaignStep
  , campaignCompletedSteps :: [CampaignStep]
  , campaignResolutions :: Map ScenarioId Resolution
  , campaignXpBreakdown :: [(CampaignStep, XpBreakdown)]
  , campaignModifiers :: Map InvestigatorId [Modifier]
  , campaignMeta :: Value
  , campaignStore :: Map Text Value
  }
  deriving stock (Show, Eq, Generic)

instance HasField "id" CampaignAttrs CampaignId where
  getField = campaignId

instance HasField "decks" CampaignAttrs (Map InvestigatorId (Deck PlayerCard)) where
  getField = campaignDecks

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

storyCardsL :: Lens' CampaignAttrs (Map InvestigatorId [PlayerCard])
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

resolutionsL :: Lens' CampaignAttrs (Map ScenarioId Resolution)
resolutionsL = lens campaignResolutions $ \m x -> m {campaignResolutions = x}

xpBreakdownL :: Lens' CampaignAttrs [(CampaignStep, XpBreakdown)]
xpBreakdownL = lens campaignXpBreakdown $ \m x -> m {campaignXpBreakdown = x}

completeStep :: CampaignStep -> [CampaignStep] -> [CampaignStep]
completeStep step' steps = step' : steps

modifiersL :: Lens' CampaignAttrs (Map InvestigatorId [Modifier])
modifiersL = lens campaignModifiers $ \m x -> m {campaignModifiers = x}

instance Entity CampaignAttrs where
  type EntityId CampaignAttrs = CampaignId
  type EntityAttrs CampaignAttrs = CampaignAttrs
  toId = campaignId
  toAttrs = id
  overAttrs f = f

addRandomBasicWeaknessIfNeeded
  :: CardGen m => ClassSymbol -> Int -> Deck PlayerCard -> m (Deck PlayerCard, [Card])
addRandomBasicWeaknessIfNeeded investigatorClass playerCount deck = do
  let
    multiplayerFilter =
      if playerCount < 2
        then notElem MultiplayerOnly . cdDeckRestrictions
        else const True
    notForClass = \case
      OnlyClass c -> c /= investigatorClass
      _ -> True
    classOnlyFilter = not . any notForClass . cdDeckRestrictions
    weaknessFilter = and . sequence [multiplayerFilter, classOnlyFilter]
  runWriterT $ do
    Deck <$> flip filterM (unDeck deck) \card -> do
      when (toCardDef card == randomWeakness) do
        sample (NE.fromList $ filter weaknessFilter allBasicWeaknesses) >>= lift . genCard >>= tell . pure
      pure $ toCardDef card /= randomWeakness

campaignWith
  :: (CampaignAttrs -> a)
  -> CampaignId
  -> Text
  -> Difficulty
  -> [ChaosTokenFace]
  -> (CampaignAttrs -> CampaignAttrs)
  -> a
campaignWith f campaignId' name difficulty chaosBagContents g = campaign (f . g) campaignId' name difficulty chaosBagContents

campaign
  :: (CampaignAttrs -> a)
  -> CampaignId
  -> Text
  -> Difficulty
  -> [ChaosTokenFace]
  -> a
campaign f campaignId' name difficulty chaosBagContents =
  f
    $ CampaignAttrs
      { campaignId = campaignId'
      , campaignName = name
      , campaignDecks = mempty
      , campaignStoryCards = mempty
      , campaignDifficulty = difficulty
      , campaignChaosBag = chaosBagContents
      , campaignLog = mkCampaignLog
      , campaignStep = PrologueStep
      , campaignCompletedSteps = []
      , campaignResolutions = mempty
      , campaignModifiers = mempty
      , campaignMeta = Null
      , campaignStore = mempty
      , campaignXpBreakdown = mempty
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

instance HasAbilities Campaign where
  getAbilities _ = []

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

oldBreakdown :: Map ScenarioId XpBreakdown -> [(CampaignStep, XpBreakdown)]
oldBreakdown = map (first ScenarioStep) . Map.toList

instance FromJSON CampaignAttrs where
  parseJSON = withObject "CampaignAttrs" $ \o -> do
    campaignId <- o .: "id"
    campaignName <- o .: "name"
    campaignDecks <- o .: "decks"
    campaignStoryCards <- o .: "storyCards"
    campaignDifficulty <- o .: "difficulty"
    campaignChaosBag <- o .: "chaosBag"
    campaignLog <- o .: "log"
    campaignStep <- o .: "step"
    campaignCompletedSteps <- o .: "completedSteps"
    campaignResolutions <- o .: "resolutions"
    campaignXpBreakdown <- (oldBreakdown <$> o .: "xpBreakdown") <|> (o .:? "xpBreakdown" .!= mempty)
    campaignModifiers <- o .: "modifiers"
    campaignMeta <- o .: "meta"
    campaignStore <- o .:? "store" .!= mempty

    pure CampaignAttrs {..}
