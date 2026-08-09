module Arkham.Act.Cards.BlackwatersBaneEpicMultiplayer (blackwatersBaneEpicMultiplayer) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Epic.Types (SharedKey (BlobStorySeed), sharedKeyText)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Log (scenarioCount, scenarioCountIncrement)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Story
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Placement
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances, EpicShared))
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Ooze, Oozified))

-- Epic Multiplayer variant of Blackwater's Bane (card 85008). Unlike Expose the
-- Anomaly, this act has NO global clue threshold: it advances at the end of the
-- round, exactly like the single-group card. The only shared behavior is the
-- organizer's story-card selection. A per-event seed plus the local cycle number
-- gives every group the same deterministic random card for a given cycle without
-- requiring a cross-group gameplay-message injection.
newtype BlackwatersBaneEpicMultiplayer = BlackwatersBaneEpicMultiplayer ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackwatersBaneEpicMultiplayer :: ActCard BlackwatersBaneEpicMultiplayer
blackwatersBaneEpicMultiplayer = act (3, A) BlackwatersBaneEpicMultiplayer Cards.blackwatersBaneEpicMultiplayer Nothing

instance HasModifiersFor BlackwatersBaneEpicMultiplayer where
  getModifiersFor (BlackwatersBaneEpicMultiplayer a) =
    modifySelect a (EnemyWithTrait Ooze) [AddKeyword Keyword.Retaliate, ScenarioModifier "noBlob"]

instance HasAbilities BlackwatersBaneEpicMultiplayer where
  getAbilities (BlackwatersBaneEpicMultiplayer a) =
    -- "A Moment of Respite": do not advance during the round in which this act
    -- entered play. Once a later round begins, advance at that round's end.
    [mkAbility a 1 (Objective $ forced $ RoundEnds #when) | toResultDefault False a.meta]

instance RunMessage BlackwatersBaneEpicMultiplayer where
  runMessage msg a@(BlackwatersBaneEpicMultiplayer attrs) = runQueueT $ case msg of
    Do BeginRound -> pure $ BlackwatersBaneEpicMultiplayer $ attrs & metaL .~ toJSON True
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ NextAdvanceActStep attrs.id 1
      pure a
    NextAdvanceActStep aid 1 | aid == attrs.id -> do
      -- Record the cycle before starting the normal multi-step act advance. This
      -- is deliberately a separate pre-advance message: story resolution on 3b
      -- can park on an Ask before later bookkeeping messages are drained.
      scenarioCountIncrement (EpicActAdvances 3)
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Only on the first advance: shuffle the set-aside Mi-Go Drones into the
      -- encounter deck along with the encounter discard pile.
      drones <- getSetAsideCardsMatching (cardIs Enemies.miGoDrone)
      unless (null drones) do
        shuffleSetAsideIntoEncounterDeck $ cardIs Enemies.miGoDrone
        shuffleEncounterDiscardBackIn

      n <- perPlayer 1
      selectEach (RevealedLocation <> LocationWithTrait Oozified <> LocationNotAtClueLimit) \loc ->
        push $ PlaceCluesUpToClueValue loc (toSource attrs) n

      -- All groups use the same story for their Nth Act 3 resolution. Increment
      -- before selecting so the first cycle is wave 1.
      wave <- scenarioCount (EpicActAdvances 3)
      seed <- scenarioCount (EpicShared (sharedKeyText BlobStorySeed))
      lead <- getLead
      let chosen = case (seed + wave) `mod` 4 of
            0 -> Stories.rescueTheChemist
            1 -> Stories.recoverTheSample
            2 -> Stories.driveOffTheMiGo
            _ -> Stories.defuseTheExplosives
      readStoryWithPlacement_ lead chosen Global
      push $ ResetActDeckToStage 1
      pure a
    _ -> BlackwatersBaneEpicMultiplayer <$> liftRunMessage msg attrs
