module Arkham.Campaigns.TheDrownedCity.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (HasQueue, push)
import Arkham.Classes.Query
import Arkham.Effect.Types (makeEffectBuilder)
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (Message (CreateEffect, DecreaseFloodLevel, IncreaseFloodLevel))
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Source
import Arkham.Tracing

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theDrownedCity" a

investigatorHasTask
  :: (HasGame m, Tracing m, HasCardDef card) => InvestigatorId -> card -> m Bool
investigatorHasTask iid (toCardDef -> cardDef) = do
  taskInPlay <- selectAny $ AssetControlledBy (InvestigatorWithId iid) <> assetIs cardDef
  campaignCards <- findWithDefault [] iid <$> getCampaignStoryCards
  pure $ taskInPlay || any ((== cardDef) . toCardDef) campaignCards

struggleForAir
  :: (Sourceable a, HasGame m, Tracing m, HasQueue Message m) => a -> InvestigatorId -> m ()
struggleForAir a iid = do
  builder <- makeEffectBuilder "struggleForAir" Nothing a iid
  push $ CreateEffect builder

decreaseFloodLevel :: ReverseQueue m => LocationId -> m ()
decreaseFloodLevel = push . DecreaseFloodLevel

increaseFloodLevel :: ReverseQueue m => LocationId -> m ()
increaseFloodLevel = push . IncreaseFloodLevel
