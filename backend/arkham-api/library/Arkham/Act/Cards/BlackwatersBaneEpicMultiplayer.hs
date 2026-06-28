module Arkham.Act.Cards.BlackwatersBaneEpicMultiplayer (blackwatersBaneEpicMultiplayer) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Epic.Types (SharedKey (BlobStorySeed, SharedActProgress), sharedKeyText)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Investigator (getSpendableClueCount)
import Arkham.Helpers.Log (scenarioCount, scenarioCountIncrement)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Story
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Placement
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicActAdvances, EpicShared))
import Arkham.Story.Cards qualified as Stories
import Arkham.Token (Token (Clue))
import Arkham.Trait (Trait (Ooze, Oozified))

-- Epic Multiplayer variant of Blackwater's Bane (card 85008). Like the Epic
-- variant of Act 1, the clue requirement is a single GLOBAL pool shared across
-- every group: 2 clues per investigator across ALL groups (the event's frozen
-- total). Unlike the single-group act (which advances at the end of a round via
-- "A Moment of Respite"), the Epic variant advances by pooled clues: each
-- investigator has a fast ability to place up to 3 of their own clues onto this
-- act, and only clues placed on a group's act feed the shared @act-progress:3@
-- counter.
--
-- ADVANCE is SEAM-COORDINATED. The cross-group seam detects @pool >= 2 * total@,
-- resets the pool, and delivers @ResolveEpicActAdvance 3 spendAmount@ to this act
-- in every group. We consume @spendAmount@ from the act, hand the leftover clues
-- to this group's investigators, loop the deck back to act 1 directly (no
-- AdvanceAct confirmation, which would park in non-interactively synced groups),
-- and read the seeded Part-1 story. We never touch a Shared* counter here -- the
-- seam owns the pool reset. @EpicActAdvances 3@ is still bumped on advance so the
-- story pick's @wave@ keeps re-rolling.
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
-- @EpicShared "act-progress:3"@ and raised on each clue placement.
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
    -- CONTRIBUTION: fast, per investigator. Place up to 3 of your clues on the act.
    [restricted a 1 (DuringTurn You <> youExist InvestigatorWithAnyClues) $ FastAbility Free]

instance RunMessage BlackwatersBaneEpicMultiplayer where
  runMessage msg a@(BlackwatersBaneEpicMultiplayer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- min 3 <$> getSpendableClueCount iid
      when (n > 0) $ chooseAmount iid "Clues" "Clues" 1 n attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Clues" -> amount) (isTarget attrs -> True) | amount > 0 -> do
      -- Place this investigator's chosen clues onto the act, then add them to the
      -- shared pool. Only clues placed on the act count.
      moveTokens (attrs.ability 1) iid attrs Clue amount
      push $ RaiseShared actProgressKey amount
      pure a
    ResolveEpicActAdvance stage spendAmount | stage == actStage -> do
      -- The seam has consumed `spendAmount` from the global pool; the leftover clues
      -- sitting on THIS group's act go to this group's investigators (they take
      -- control). The act's own clue tokens are discarded by the deck reset below;
      -- we re-grant only the leftover, so `spendAmount` is effectively spent.
      let leftover = max 0 (attrs.clues - spendAmount)
      when (leftover > 0) do
        iids <- select UneliminatedInvestigator
        unless (null iids) do
          let numInvestigators = length iids
              base = leftover `div` numInvestigators
              extra = leftover `mod` numInvestigators
          for_ (zip [0 ..] iids) \(i, iid) -> do
            let amt = base + if i < extra then 1 else 0
            when (amt > 0) $ gainClues iid (toSource attrs) amt
      -- This advance is the (advances + 1)th time this group has reached 3b; read
      -- the count BEFORE the increment so `wave` is unambiguously the Kth.
      wave <- (+ 1) <$> scenarioCount actAdvancesKey
      scenarioCountIncrement actAdvancesKey
      -- Only on the first advance: shuffle the set-aside Mi-Go Drones into the
      -- encounter deck along with the encounter discard pile.
      drones <- getSetAsideCardsMatching (cardIs Enemies.miGoDrone)
      unless (null drones) do
        shuffleSetAsideIntoEncounterDeck $ cardIs Enemies.miGoDrone
        shuffleEncounterDiscardBackIn
      n <- perPlayer 1
      selectEach (RevealedLocation <> LocationWithTrait Oozified <> LocationNotAtClueLimit) \loc ->
        push $ PlaceCluesUpToClueValue loc (toSource attrs) n
      -- Loop the deck back to act 1 directly (no AdvanceAct confirmation) BEFORE the
      -- story read, so the deck fully resets even when an idle group is synced
      -- non-interactively (the story resolution below parks).
      push $ ResetActDeckToStage 1
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
      pure a
    _ -> BlackwatersBaneEpicMultiplayer <$> liftRunMessage msg attrs
