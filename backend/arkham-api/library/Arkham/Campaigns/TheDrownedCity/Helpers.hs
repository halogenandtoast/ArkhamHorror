module Arkham.Campaigns.TheDrownedCity.Helpers where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog (campaignLogRecordedCounts)
import Arkham.CampaignLogKey (IsCampaignLogKey, toCampaignLogKey)
import Arkham.Campaigns.TheDrownedCity.Key
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (HasQueue, push)
import Arkham.Classes.Query
import Arkham.Effect.Types (makeEffectBuilder)
import Arkham.GameValue
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Helpers.Log (getHasRecord, getSomeRecordSet)
import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorLog))
import Arkham.Matcher
import Arkham.Message (Message (CreateEffect, DecreaseFloodLevel, IncreaseFloodLevel))
import Arkham.Message.Lifted (takeControlOfAsset)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Queue
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection (fieldMap)
import Arkham.Source
import Arkham.Target
import Arkham.Tracing
import Control.Monad.Writer.Class
import Data.Char qualified as C
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Text qualified as T

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theDrownedCity" a

{- | "Artifacts have a unique card back, have no cost, cannot leave play, and cannot
be chosen to be discarded via scenario effects." Nothing can send one to a discard
pile it has no back for, so anything that would discard it removes it instead.
-}
artifactModifiers
  :: ( Targetable a
     , Sourceable a
     , HasGame m
     , Tracing m
     , MonadWriter (MonoidalMap Target [Modifier]) m
     )
  => a
  -> m ()
artifactModifiers a = modifySelf a [CannotLeavePlay, RemoveFromGameInsteadOfDiscard]

{- | "If any investigator in control of an artifact is defeated, give control of it
to the nearest surviving investigator instead of removing it from the game." Silent
because there is no decision to make while only one investigator is nearest, and the
artifact never leaves play either way. Resolve with 'handOffArtifact'.
-}
artifactAbility :: (Sourceable a, HasCardCode a) => a -> Int -> Ability
artifactAbility a n =
  controlled a n (exists $ not_ You)
    $ SilentForcedAbility
    $ InvestigatorDefeated #when ByAny You

-- | Resolves 'artifactAbility'; @iid@ is the investigator being defeated.
handOffArtifact
  :: (ReverseQueue m, ToId asset AssetId) => InvestigatorId -> asset -> m ()
handOffArtifact iid asset = do
  nearest <-
    select $ not_ (InvestigatorWithId iid) <> NearestToLocation (locationWithInvestigator iid)
  chooseOrRunOneM iid $ targets nearest (`takeControlOfAsset` asId asset)

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

{- | Task progress is recorded in each investigator's own log so it can be shown
per-investigator; read it back from there rather than the campaign log.
-}
getRecordCountForInvestigator
  :: (HasGame m, Tracing m, IsCampaignLogKey k) => InvestigatorId -> k -> m Int
getRecordCountForInvestigator iid k =
  fieldMap InvestigatorLog (findWithDefault 0 (toCampaignLogKey k) . campaignLogRecordedCounts) iid

struggleForAir
  :: (Sourceable a, HasGame m, Tracing m, HasQueue Message m) => a -> InvestigatorId -> m ()
struggleForAir a iid = do
  builder <- makeEffectBuilder "struggleForAir" Nothing a iid
  push $ CreateEffect builder

decreaseFloodLevel :: ReverseQueue m => LocationId -> m ()
decreaseFloodLevel = push . DecreaseFloodLevel

increaseFloodLevel :: ReverseQueue m => LocationId -> m ()
increaseFloodLevel = push . IncreaseFloodLevel

{- | The scenarios printed on the R'lyeh map. The whole set is recorded at
campaign start and entries are crossed out as each scenario is completed.
-}
data RlyehMapEntry
  = -- Prefixed because the bare names collide with the CampaignSteps patterns,
    -- the EncounterSet constructors, and the scenario newtypes.
    RlyehWesternWall
  | RlyehDrownedQuarter
  | RlyehApiary
  | RlyehGrandVault
  | RlyehCourtOfTheAncients
  | RlyehObsidianCanyons
  | RlyehSepulchreOfTheSleeper
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

type Glyph = Text

getKnownGlyphs :: (HasGame m, Tracing m) => m Text
getKnownGlyphs = getSomeRecordSet DiscoveredGlyphs <&> \xs -> mconcat [x | String x <- xs]

{- | Glyphs are recorded uppercase (@glyphLetter@ upper-cases before inserting into
the @DiscoveredGlyphs@ set), so normalize both sides: a card asking for @"qxgks"@
must match a record set holding @"QXGKS"@.
-}
getGlyphsAllKnown :: (HasGame m, Tracing m) => String -> m Bool
getGlyphsAllKnown xs = do
  ys <- T.toUpper <$> getKnownGlyphs
  pure $ all ((`T.elem` ys) . C.toUpper) xs

{- | Criterion form of 'getGlyphsAllKnown', for abilities that only come online
once their glyphs have been translated. @HasAbilities@ is pure and cannot query
the campaign log, so this defers the lookup to criteria evaluation instead of
round-tripping the answer through a marker modifier. "All of them" is
'recordSetHasAtLeast' with the (deduplicated) list's own length as the threshold.
-}
glyphsAllKnown :: String -> Criterion
glyphsAllKnown xs = recordSetHasAtLeast (Static $ length letters) DiscoveredGlyphs letters
 where
  letters = ordNub $ map (T.singleton . C.toUpper) xs
