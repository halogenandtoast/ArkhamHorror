module Arkham.Enemy.CardDefs.TheForgottenAge.TheUntamedWilds where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

ichtaca :: CardDef
ichtaca =
  unique
    $ (enemy "04052" ("Ichtaca" <:> "Keeper of the Eztli") TheUntamedWilds 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 5
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Eztli, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }
