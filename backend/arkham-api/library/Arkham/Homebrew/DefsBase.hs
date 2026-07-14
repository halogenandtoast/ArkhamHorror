{-# LANGUAGE AllowAmbiguousTypes #-}

module Arkham.Homebrew.DefsBase where

import Arkham.Card.CardDef
import Arkham.Prelude

-- | Everything a homebrew campaign (or standalone) contributes to the card
-- definition tables, one slot per @all*Cards@ aggregate.
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
      }

instance Monoid HomebrewDefs where
  mempty = HomebrewDefs [] [] [] [] [] [] [] [] []

-- | Implement in your campaign's @Defs.hs@ on a campaign-local tag type; the
-- instance is discovered automatically (see 'Arkham.Homebrew.Defs').
class IsHomebrewDefs a where
  homebrewDefs :: HomebrewDefs
