module Arkham.Story.CardDefs.TheDrownedCity.TheDrownedQuarter where

import Arkham.Story.CardDefs.Import

obsidianRelic :: CardDef
obsidianRelic =
  (doubleSided $ story "11550b" "Obsidian Relic" TheDrownedQuarter)
    { cdVictoryPoints = Just 1
    }

underseaParasite :: CardDef
underseaParasite =
  (doubleSided $ story "11549b" "Undersea Parasite" TheDrownedQuarter)
    { cdVictoryPoints = Just 1
    }
