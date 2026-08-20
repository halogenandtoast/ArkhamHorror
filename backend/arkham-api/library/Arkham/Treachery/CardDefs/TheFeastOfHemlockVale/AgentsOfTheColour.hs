module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.AgentsOfTheColour where

import Arkham.Treachery.CardDefs.Import

alienWhispers :: CardDef
alienWhispers =
  (treachery "10725" "Alien Whispers" AgentsOfTheColour 2)
    { cdCardTraits = setFromList [Power, Colour]
    }
