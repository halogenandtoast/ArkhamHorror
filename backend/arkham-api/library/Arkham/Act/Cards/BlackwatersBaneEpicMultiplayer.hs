module Arkham.Act.Cards.BlackwatersBaneEpicMultiplayer (blackwatersBaneEpicMultiplayer) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Epic.Types (SharedKey (BlobStorySeed, SharedActProgress), sharedKeyText, totalInvestigatorsKey)
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

-- Epic Multiplayer variant of Blackwater's Bane (card 85008). Like the Epic
-- variant of Act 1, the clue requirement is a single GLOBAL pool shared across
-- every group: 2 clues per investigator across ALL groups (the event's frozen
-- total). Unlike the single-group act (which advances at the end of a round via
-- "A Moment of Respite"), the Epic variant advances by pooled clues: each group
-- contributes its local share into the shared @act-progress:3@ counter at the
-- start of each round.
--
-- The shared pool is CUMULATIVE and never reset; each group tracks how many times
-- this act has advanced in a LOCAL @EpicActAdvances 3@ scenario count, and the Nth
-- advance fires once the cumulative pool reaches @2 * total * N@. Because this act
-- loops ('ResetActDeckToStage 1' brings the deck back round to it), the cumulative
-- threshold makes every pass require another 2*total fresh clues, in lockstep
-- across all groups (see the Act 1 variant for the full rationale).
newtype BlackwatersBaneEpicMultiplayer = BlackwatersBaneEpicMultiplayer ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackwatersBaneEpicMultiplayer :: ActCard BlackwatersBaneEpicMultiplayer
blackwatersBaneEpicMultiplayer = act (3, A) BlackwatersBaneEpicMultiplayer Cards.blackwatersBaneEpicMultiplayer Nothing

-- This act's stage; both the shared progress key and the local advance-count key
-- derive from it so they can't drift.
actStage :: Int
actStage = 3

-- The shared act-progress 'SharedKey', mirrored into scenario state as
-- @EpicShared "act-progress:3"@.
actProgressKey :: SharedKey
actProgressKey = SharedActProgress actStage

-- The LOCAL (per-group, never-synced) count of how many times this act has advanced.
actAdvancesKey :: ScenarioCountKey
actAdvancesKey = EpicActAdvances actStage

instance HasModifiersFor BlackwatersBaneEpicMultiplayer where
  getModifiersFor (BlackwatersBaneEpicMultiplayer a) =
    modifySelect a (EnemyWithTrait Ooze) [AddKeyword Keyword.Retaliate, ScenarioModifier "noBlob"]

instance HasAbilities BlackwatersBaneEpicMultiplayer where
  getAbilities (BlackwatersBaneEpicMultiplayer a) =
    [ -- CONTRIBUTION: round-start group-clue payment (mirrored from Act 1), feeding
      -- the shared pool instead of advancing this group's act directly.
      mkAbility a 1 $ Objective $ triggered (RoundBegins #when) $ GroupClueCost (PerPlayer 2) Anywhere
    , -- AUTO-ADVANCE: an engine hook on the shared-counter mirror. The act advances
      -- here once the pooled total reaches the global threshold.
      mkAbility a 2
        $ SilentForcedAbility
        $ ScenarioCountIncremented #after (EpicShared (sharedKeyText actProgressKey))
    ]

instance RunMessage BlackwatersBaneEpicMultiplayer where
  runMessage msg a@(BlackwatersBaneEpicMultiplayer attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- Contribute this group's clues (2 per local player) to the shared pool.
      n <- perPlayer 2
      push $ RaiseShared actProgressKey n
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      progress <- scenarioCount (EpicShared (sharedKeyText actProgressKey))
      total <- scenarioCount (EpicShared totalInvestigatorsKey)
      advances <- scenarioCount actAdvancesKey
      -- Cumulative threshold: the (advances + 1)th advance needs 2 * total clues
      -- beyond all prior advances.
      when (progress >= 2 * total * (advances + 1)) $ advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- This advance is the (advances + 1)th time THIS group has reached 3b; read
      -- the count BEFORE the increment below so `wave` is unambiguously the Kth
      -- regardless of when the queued increment applies.
      wave <- (+ 1) <$> scenarioCount actAdvancesKey
      -- Record this advance locally so the next pass needs another 2 * total clues
      -- (and the shared story re-rolls).
      scenarioCountIncrement actAdvancesKey
      -- Only on the first advance: shuffle the set-aside Mi-Go Drones into the
      -- encounter deck along with the encounter discard pile.
      drones <- getSetAsideCardsMatching (cardIs Enemies.miGoDrone)
      unless (null drones) do
        shuffleSetAsideIntoEncounterDeck $ cardIs Enemies.miGoDrone
        shuffleEncounterDiscardBackIn
      n <- perPlayer 1
      selectEach (RevealedLocation <> LocationWithTrait Oozified <> LocationNotAtClueLimit) \loc -> do
        push $ PlaceCluesUpToClueValue loc (toSource attrs) n

      -- Shared Part-1 story: every group derives the SAME pick from a single
      -- per-event seed (EpicShared "blob-story-seed", synced to all groups) plus
      -- this group's wave. All groups start at wave 1 and the seed is identical,
      -- so the first 3b in every group reads the same card; each loop (wave 2,
      -- 3, ...) re-rolls to a different one. `mod 4` is always 0..3.
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
