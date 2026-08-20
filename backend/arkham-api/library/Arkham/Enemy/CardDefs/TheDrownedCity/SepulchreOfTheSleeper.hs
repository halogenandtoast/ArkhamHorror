module Arkham.Enemy.CardDefs.TheDrownedCity.SepulchreOfTheSleeper where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

cthulhuDeadAndDreaming :: CardDef
cthulhuDeadAndDreaming =
  unique
    $ doubleSided "11674"
    $ (enemy "11674b" ("Cthulhu" <:> "Dead and Dreaming") SepulchreOfTheSleeper 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdHealth = healthPerInvestigator 20
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Relentless]
      , cdVictoryPoints = Just 5
      }
