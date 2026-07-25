module Arkham.Campaigns.TheDrownedCity.Helpers where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDrownedCity.Key
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (HasQueue, push)
import Arkham.Classes.Query
import Arkham.Effect.Types (makeEffectBuilder)
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Helpers.Log (getHasRecord)
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

{- | Each Artifact story asset paired with the Campaign Log record that marks it
as earned. Several scenarios let players begin play with an earned Artifact.
-}
artifactAssets :: [(TheDrownedCityKey, CardDef)]
artifactAssets =
  [ (BarrierNode, Assets.barrierNode)
  , (ObsidianClaw, Assets.obsidianClaw)
  , (ShardOfYchlecht, Assets.shardOfYchlecht)
  , (TidalTablet, Assets.tidalTablet)
  , (GrislyMask, Assets.grislyMask)
  , (HorrorInClay, Assets.horrorInClay)
  ]

getEarnedArtifacts :: (HasGame m, Tracing m) => m [CardDef]
getEarnedArtifacts = map snd <$> filterM (getHasRecord . fst) artifactAssets

{- | The @Item@ assets in the /Expedition/ encounter set. Ruby Standish and Andy
Van Nortwick are in the set too, but they are @Ally@ assets.
-}
expeditionItems :: [CardDef]
expeditionItems =
  [ Assets.expeditionGear
  , Assets.laudanum
  , Assets.alienTablet
  , Assets.divingSuitTheDrownedCity
  ]

-- | Each Task: campaign-log key, the story-asset card, and its i18n label.
tasks :: [(TheDrownedCityKey, CardDef, Text)]
tasks =
  [ (WalkInFaith, Assets.walkInFaith, "walkInFaith")
  , (ToeTheLine, Assets.toeTheLine, "toeTheLine")
  , (NoPlaceLikeHome, Assets.noPlaceLikeHome, "noPlaceLikeHome")
  , (GoodMoney, Assets.goodMoney, "goodMoney")
  , (DoNoHarm, Assets.doNoHarm, "doNoHarm")
  , (ProveYourWorth, Assets.proveYourWorth, "proveYourWorth")
  , (DreamsOfDestruction, Assets.dreamsOfDestruction, "dreamsOfDestruction")
  , (PlumbTheDepths, Assets.plumbTheDepths, "plumbTheDepths")
  ]

{- | The Tasks an investigator has taken. Each investigator takes exactly one, but
this returns a list so callers do not have to assume that.
-}
getInvestigatorTasks
  :: (HasGame m, Tracing m) => InvestigatorId -> m [(TheDrownedCityKey, CardDef, Text)]
getInvestigatorTasks iid = filterM (\(_, def, _) -> investigatorHasTask iid def) tasks

struggleForAir
  :: (Sourceable a, HasGame m, Tracing m, HasQueue Message m) => a -> InvestigatorId -> m ()
struggleForAir a iid = do
  builder <- makeEffectBuilder "struggleForAir" Nothing a iid
  push $ CreateEffect builder

decreaseFloodLevel :: ReverseQueue m => LocationId -> m ()
decreaseFloodLevel = push . DecreaseFloodLevel

increaseFloodLevel :: ReverseQueue m => LocationId -> m ()
increaseFloodLevel = push . IncreaseFloodLevel
