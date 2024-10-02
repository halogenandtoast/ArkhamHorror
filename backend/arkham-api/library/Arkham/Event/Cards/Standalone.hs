module Arkham.Event.Cards.Standalone where

import Arkham.Event.Cards.Import

regurgitation :: CardDef
regurgitation =
  signature "89001"
    $ (event "89003" "Regurgitation" 0 Neutral)
      { cdCardTraits = setFromList [Power]
      , cdSkills = [#wild]
      , cdFastWindow = Just $ DuringTurn You
      }
