module Arkham.Enemy.CardDefs.TheCircleUndone.AgentsOfAzathoth where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

piperOfAzathoth :: CardDef
piperOfAzathoth =
  (enemy "05088" "Piper of Azathoth" AgentsOfAzathoth 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 2
    , cdHealth = health 7
    , cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }
