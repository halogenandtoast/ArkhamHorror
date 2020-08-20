{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Campaign.Attrs where

import Arkham.Json
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignId
import Arkham.Types.CampaignLog
import Arkham.Types.CampaignStep
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Investigator
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Token
import ClassyPrelude hiding (log)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro

data Attrs = Attrs
  { campaignId :: CampaignId
  , campaignName :: Text
  , campaignInvestigators :: HashMap Int Investigator
  , campaignDecks :: HashMap InvestigatorId [PlayerCard]
  , campaignDifficulty :: Difficulty
  , campaignChaosBag :: [Token]
  , campaignLog :: CampaignLog
  , campaignSteps :: Vector CampaignStep
  , campaignStep :: Int
  }
  deriving stock (Show, Generic)

decks :: Lens' Attrs (HashMap InvestigatorId [PlayerCard])
decks = lens campaignDecks $ \m x -> m { campaignDecks = x }

step :: Lens' Attrs Int
step = lens campaignStep $ \m x -> m { campaignStep = x }

log :: Lens' Attrs CampaignLog
log = lens campaignLog $ \m x -> m { campaignLog = x }

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "campaign"
  toEncoding = genericToEncoding $ aesonOptions $ Just "campaign"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "campaign"

instance (CampaignRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    InitDeck iid deck -> pure $ a & decks %~ HashMap.insert iid deck
    ResetGame -> a <$ unshiftMessages
      [ LoadDeck iid deck | (iid, deck) <- HashMap.toList campaignDecks ]
    CrossOutRecord key ->
      pure
        $ a
        & (log . recorded %~ HashSet.delete key)
        & (log . recordedSets %~ HashMap.delete key)
        & (log . recordedCounts %~ HashMap.delete key)
    Record key -> pure $ a & log . recorded %~ HashSet.insert key
    RecordSet key cardCodes ->
      pure $ a & log . recordedSets %~ HashMap.insert key cardCodes
    RecordCount key int ->
      pure $ a & log . recordedCounts %~ HashMap.insert key int
    _ -> pure a

baseAttrs :: CampaignId -> Text -> Difficulty -> [Token] -> Attrs
baseAttrs campaignId' name difficulty chaosBagContents = Attrs
  { campaignId = campaignId'
  , campaignName = name
  , campaignInvestigators = mempty
  , campaignDecks = mempty
  , campaignDifficulty = difficulty
  , campaignChaosBag = chaosBagContents
  , campaignLog = mkCampaignLog
  , campaignSteps = mempty
  , campaignStep = 0
  }
