module Arkham.Enemy.CardDefs.TheForgottenAge.TheDoomOfEztli where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

harbingerOfValusia :: CardDef
harbingerOfValusia =
  unique
    $ ( enemy
          "04062"
          ("Harbinger of Valusia" <:> "The Sleeper Awakens")
          TheDoomOfEztli
          1
      )
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 10
      , cdCardTraits = setFromList [Humanoid, Serpent, Monster, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
      , cdVengeancePoints = Just 5
      }
