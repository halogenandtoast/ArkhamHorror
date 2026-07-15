{-# LANGUAGE AllowAmbiguousTypes #-}

module Arkham.Homebrew.DefsBase where

import Arkham.Action (Action)
import Arkham.Card.CardDef
import Arkham.Criteria (Criterion)
import Arkham.Prelude
import Arkham.Trait (Trait)

-- | Everything a homebrew campaign (or standalone) contributes: its card
-- definition tables (one slot per @all*Cards@ aggregate) and its trait values.
data HomebrewDefs = HomebrewDefs
  { hdLocations :: [CardDef]
  , hdEnemies :: [CardDef]
  , hdTreacheries :: [CardDef]
  , hdPlayerTreacheries :: [CardDef]
  , hdActs :: [CardDef]
  , hdAgendas :: [CardDef]
  , hdEncounterAssets :: [CardDef]
  , hdPlayerSkills :: [CardDef]
  , hdStories :: [CardDef]
  , -- | The campaign's own trait values (see its @Traits.hs@). Folded into
    -- @Arkham.Homebrew.Defs.allTraits@ so they appear wherever the full trait
    -- universe is enumerated.
    hdTraits :: [Trait]
  , -- | The campaign's own action values (see its @Actions.hs@). Folded into
    -- @Arkham.Homebrew.Defs.allActions@.
    hdActions :: [Action]
  , -- | Per-action affordability rules for this campaign's actions, evaluated
    -- generically by core (see @Arkham.Homebrew.Defs.homebrewActionAffordability@
    -- and its use in @Arkham.Helpers.Ability.canDoAction'@). An action with no
    -- entry is always affordable.
    hdActionAffordability :: [(Action, Criterion)]
  }

instance Semigroup HomebrewDefs where
  a <> b =
    HomebrewDefs
      { hdLocations = hdLocations a <> hdLocations b
      , hdEnemies = hdEnemies a <> hdEnemies b
      , hdTreacheries = hdTreacheries a <> hdTreacheries b
      , hdPlayerTreacheries = hdPlayerTreacheries a <> hdPlayerTreacheries b
      , hdActs = hdActs a <> hdActs b
      , hdAgendas = hdAgendas a <> hdAgendas b
      , hdEncounterAssets = hdEncounterAssets a <> hdEncounterAssets b
      , hdPlayerSkills = hdPlayerSkills a <> hdPlayerSkills b
      , hdStories = hdStories a <> hdStories b
      , hdTraits = hdTraits a <> hdTraits b
      , hdActions = hdActions a <> hdActions b
      , hdActionAffordability = hdActionAffordability a <> hdActionAffordability b
      }

instance Monoid HomebrewDefs where
  mempty = HomebrewDefs [] [] [] [] [] [] [] [] [] [] [] []

-- | Implement in your campaign's @Defs.hs@ on a campaign-local tag type; the
-- instance is discovered automatically (see 'Arkham.Homebrew.Defs').
class IsHomebrewDefs a where
  homebrewDefs :: HomebrewDefs
