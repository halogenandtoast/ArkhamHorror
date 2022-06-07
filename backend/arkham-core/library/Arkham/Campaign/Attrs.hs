{-# LANGUAGE TemplateHaskell #-}
module Arkham.Campaign.Attrs where

import Arkham.Prelude

import Data.Aeson.TH
import Arkham.PlayerCard
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Classes.Entity
import Arkham.Difficulty
import Arkham.Helpers
import Arkham.Id
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Message
import Arkham.Name
import Arkham.Projection
import Arkham.Resolution
import Arkham.Token
import Arkham.Json
import Control.Monad.Writer hiding (filterM)
import Data.List.NonEmpty qualified as NE

class IsCampaign a

data instance Field CampaignAttrs :: Type -> Type where
  CampaignCompletedSteps :: Field CampaignAttrs [CampaignStep]
  CampaignStoryCards :: Field CampaignAttrs (HashMap InvestigatorId [PlayerCard])

data CampaignAttrs = CampaignAttrs
  { campaignId :: CampaignId
  , campaignName :: Text
  , campaignInvestigators :: HashMap Int Investigator
  , campaignDecks :: HashMap InvestigatorId (Deck PlayerCard)
  , campaignStoryCards :: HashMap InvestigatorId [PlayerCard]
  , campaignDifficulty :: Difficulty
  , campaignChaosBag :: [TokenFace]
  , campaignLog :: CampaignLog
  , campaignStep :: Maybe CampaignStep
  , campaignCompletedSteps :: [CampaignStep]
  , campaignResolutions :: HashMap ScenarioId Resolution
  }
  deriving stock (Show, Eq)

completedStepsL :: Lens' CampaignAttrs [CampaignStep]
completedStepsL =
  lens campaignCompletedSteps $ \m x -> m { campaignCompletedSteps = x }

chaosBagL :: Lens' CampaignAttrs [TokenFace]
chaosBagL = lens campaignChaosBag $ \m x -> m { campaignChaosBag = x }

storyCardsL :: Lens' CampaignAttrs (HashMap InvestigatorId [PlayerCard])
storyCardsL = lens campaignStoryCards $ \m x -> m { campaignStoryCards = x }

decksL :: Lens' CampaignAttrs (HashMap InvestigatorId (Deck PlayerCard))
decksL = lens campaignDecks $ \m x -> m { campaignDecks = x }

logL :: Lens' CampaignAttrs CampaignLog
logL = lens campaignLog $ \m x -> m { campaignLog = x }

stepL :: Lens' CampaignAttrs (Maybe CampaignStep)
stepL = lens campaignStep $ \m x -> m { campaignStep = x }

resolutionsL :: Lens' CampaignAttrs (HashMap ScenarioId Resolution)
resolutionsL = lens campaignResolutions $ \m x -> m { campaignResolutions = x }

completeStep :: Maybe CampaignStep -> [CampaignStep] -> [CampaignStep]
completeStep (Just step') steps = step' : steps
completeStep Nothing steps = steps

instance Entity CampaignAttrs where
  type EntityId CampaignAttrs = CampaignId
  type EntityAttrs CampaignAttrs = CampaignAttrs
  toId = campaignId
  toAttrs = id

$(deriveJSON (aesonOptions $ Just "Campaign") ''CampaignAttrs)

addRandomBasicWeaknessIfNeeded
  :: MonadRandom m => Deck PlayerCard -> m (Deck PlayerCard, [CardDef])
addRandomBasicWeaknessIfNeeded deck = runWriterT $ do
  Deck <$> flip
    filterM
    (unDeck deck)
    \card -> do
      when
        (toCardDef card == randomWeakness)
        (sample (NE.fromList allBasicWeaknesses) >>= tell . pure)
      pure $ toCardDef card /= randomWeakness

baseAttrs :: CampaignId -> Text -> Difficulty -> [TokenFace] -> CampaignAttrs
baseAttrs campaignId' name difficulty chaosBagContents = CampaignAttrs
  { campaignId = campaignId'
  , campaignName = name
  , campaignInvestigators = mempty
  , campaignDecks = mempty
  , campaignStoryCards = mempty
  , campaignDifficulty = difficulty
  , campaignChaosBag = chaosBagContents
  , campaignLog = mkCampaignLog
  , campaignStep = Just PrologueStep
  , campaignCompletedSteps = []
  , campaignResolutions = mempty
  }
