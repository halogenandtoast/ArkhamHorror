{-# OPTIONS_GHC -Wno-dodgy-imports #-}

module Arkham.Campaign.Types where

import Arkham.Prelude

import Arkham.CampaignLog
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.Entity
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
import Control.Monad.Writer hiding (filterM)
import Data.List.NonEmpty qualified as NE
import Data.Typeable

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

data instance Field Campaign :: Type -> Type where
  CampaignCompletedSteps :: Field Campaign [CampaignStep]
  CampaignStoryCards :: Field Campaign (Map InvestigatorId [PlayerCard])
  CampaignCampaignLog :: Field Campaign CampaignLog
  CampaignDecks :: Field Campaign (Map InvestigatorId (Deck PlayerCard))

data CampaignAttrs = CampaignAttrs
  { campaignId :: CampaignId
  , campaignName :: Text
  , campaignDecks :: Map InvestigatorId (Deck PlayerCard)
  , campaignStoryCards :: Map InvestigatorId [PlayerCard]
  , campaignDifficulty :: Difficulty
  , campaignChaosBag :: [ChaosTokenFace]
  , campaignLog :: CampaignLog
  , campaignStep :: Maybe CampaignStep
  , campaignCompletedSteps :: [CampaignStep]
  , campaignResolutions :: Map ScenarioId Resolution
  , campaignModifiers :: Map InvestigatorId [Modifier]
  }
  deriving stock (Show, Eq, Generic)

instance HasModifiersFor CampaignAttrs where
  getModifiersFor (InvestigatorTarget iid) attrs =
    pure $ findWithDefault [] iid (campaignModifiers attrs)
  getModifiersFor _ _ = pure []

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

stepL :: Lens' CampaignAttrs (Maybe CampaignStep)
stepL = lens campaignStep $ \m x -> m {campaignStep = x}

resolutionsL :: Lens' CampaignAttrs (Map ScenarioId Resolution)
resolutionsL = lens campaignResolutions $ \m x -> m {campaignResolutions = x}

completeStep :: Maybe CampaignStep -> [CampaignStep] -> [CampaignStep]
completeStep (Just step') steps = step' : steps
completeStep Nothing steps = steps

modifiersL :: Lens' CampaignAttrs (Map InvestigatorId [Modifier])
modifiersL = lens campaignModifiers $ \m x -> m {campaignModifiers = x}

instance Entity CampaignAttrs where
  type EntityId CampaignAttrs = CampaignId
  type EntityAttrs CampaignAttrs = CampaignAttrs
  toId = campaignId
  toAttrs = id
  overAttrs f = f

instance ToJSON CampaignAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "campaign"

instance FromJSON CampaignAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaign"

addRandomBasicWeaknessIfNeeded
  :: (MonadRandom m) => Deck PlayerCard -> m (Deck PlayerCard, [CardDef])
addRandomBasicWeaknessIfNeeded deck = runWriterT $ do
  Deck <$> flip
    filterM
    (unDeck deck)
    \card -> do
      when
        (toCardDef card == randomWeakness)
        (sample (NE.fromList allBasicWeaknesses) >>= tell . pure)
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
  f $
    CampaignAttrs
      { campaignId = campaignId'
      , campaignName = name
      , campaignDecks = mempty
      , campaignStoryCards = mempty
      , campaignDifficulty = difficulty
      , campaignChaosBag = chaosBagContents
      , campaignLog = mkCampaignLog
      , campaignStep = Just PrologueStep
      , campaignCompletedSteps = []
      , campaignResolutions = mempty
      , campaignModifiers = mempty
      }

instance Entity Campaign where
  type EntityId Campaign = CampaignId
  type EntityAttrs Campaign = CampaignAttrs
  toId = toId . toAttrs
  toAttrs (Campaign a) = toAttrs a
  overAttrs f (Campaign a) = Campaign $ overAttrs f a

instance Targetable Campaign where
  toTarget _ = CampaignTarget

data Campaign = forall a. (IsCampaign a) => Campaign a

instance Eq Campaign where
  (Campaign (a :: a)) == (Campaign (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Campaign where
  show (Campaign a) = show a

instance ToJSON Campaign where
  toJSON (Campaign a) = toJSON a

instance HasModifiersFor Campaign where
  getModifiersFor target (Campaign a) = getModifiersFor target a

difficultyOf :: Campaign -> Difficulty
difficultyOf = campaignDifficulty . toAttrs

chaosBagOf :: Campaign -> [ChaosTokenFace]
chaosBagOf = campaignChaosBag . toAttrs
