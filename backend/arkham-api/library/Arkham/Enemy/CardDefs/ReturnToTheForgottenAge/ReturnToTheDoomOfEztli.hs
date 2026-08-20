module Arkham.Enemy.CardDefs.ReturnToTheForgottenAge.ReturnToTheDoomOfEztli where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

harbingerOfValusiaTheSleeperReturns :: CardDef
harbingerOfValusiaTheSleeperReturns =
  unique
    $ ( enemy
          "53018"
          ("Harbinger of Valusia" <:> "The Sleeper Returns")
          ReturnToTheDoomOfEztli
          1
      )
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 10
      , cdCardTraits = setFromList [Humanoid, Serpent, Monster, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
      , cdVengeancePoints = Just 5
      }
