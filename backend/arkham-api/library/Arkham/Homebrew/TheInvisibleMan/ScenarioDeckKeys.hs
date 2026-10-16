{-# LANGUAGE TemplateHaskell #-}

{- | The two story decks Griffin's invisibility is fought through. Every attack
against him draws from the Invisible Combat deck and every evasion from the
Invisible Escape deck; the acts shuffle cards in and out of them as the
investigators learn his habits.
-}
module Arkham.Homebrew.TheInvisibleMan.ScenarioDeckKeys (module Arkham.Homebrew.TheInvisibleMan.ScenarioDeckKeys) where

import Arkham.Homebrew.TH (declareHomebrewScenarioDeckKeys)

declareHomebrewScenarioDeckKeys
  [ "InvisibleCombatDeck"
  , "InvisibleEscapeDeck"
  ]
