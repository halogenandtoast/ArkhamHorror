{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.History where

import Arkham.Id
import Arkham.Matcher.Card
import Arkham.Matcher.Location
import Arkham.Matcher.Value
import Arkham.Prelude
import Arkham.Trait (Trait)
import Data.Aeson.TH

{- | What an investigator did over a phase, a turn or a round.

One constructor per thing 'Arkham.History.History' records, because that is the
whole of what can be asked: a card that wants anything else wants a reading the
game is not keeping, and the honest answer is to record it rather than to
approximate it here. Counts take a 'ValueMatcher' so "1 or more" and "3 or more"
are the same question asked twice.
-}
data HistoryMatcher
  = DefeatedEnemiesWithTotalHealth ValueMatcher
  | DefeatedEnemyWithTraitAt Trait LocationId
  | AttackedByAnyEnemies
  | AttackedByEnemy EnemyId
  | CluesDiscoveredAt ValueMatcher LocationId
  | -- | Clues discovered anywhere, added up.
    CluesDiscovered ValueMatcher
  | -- | A drawn treachery or enemy is remembered by card code alone, so only
    -- what a def answers can be asked of one: how many, and their traits.
    DrewTreacheries ValueMatcher
  | DrewEnemies ValueMatcher
  | DrewTreacheryWithTrait Trait
  | DrewEnemyWithTrait Trait
  | PlayedCardsMatching ValueMatcher CardMatcher
  | SkillTestsPerformed ValueMatcher
  | SuccessfulSkillTests ValueMatcher
  | FailedSkillTests ValueMatcher
  | ActionsCompleted ValueMatcher
  | ActionsSpent ValueMatcher
  | TimesMoved ValueMatcher
  | -- | "If you moved to a location with an enemy this round."
    MovedToLocation LocationMatcher
  | -- | "If you revealed or put a location into play this round."
    RevealedLocations ValueMatcher
  | ResourcesGained ValueMatcher
  | CardsDrawn ValueMatcher
  | SuccessfulAttacks ValueMatcher
  | SuccessfulEvasions ValueMatcher
  | SuccessfulInvestigations ValueMatcher
  | AttacksOfOpportunityTaken ValueMatcher
  | SuccessfullyInvestigated LocationMatcher
  | SuccessfullyExplored
  | DealtDamageToAnything
  | HistoryOneOf [HistoryMatcher]
  | HistoryAll [HistoryMatcher]
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''HistoryMatcher)
