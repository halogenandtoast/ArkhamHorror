module Arkham.Enemy.CardDefs.TheCircleUndone.TheWitchingHour where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

anetteMason :: CardDef
anetteMason =
  unique
    $ (enemy "05057" ("Anette Mason" <:> "The High Priestess") TheWitchingHour 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Humanoid, Witch, Elite]
      , cdKeywords = singleton Keyword.Retaliate
      , cdVictoryPoints = Just 2
      }
