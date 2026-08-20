module Arkham.Enemy.CardDefs.TheDunwichLegacy.ExtracurricularActivity where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

theExperiment :: CardDef
theExperiment =
  unique
    $ ( enemy
          "02058"
          ("The Experiment" <:> "Something Went Terribly Wrong")
          ExtracurricularActivity
          1
      )
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 7
      , cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Massive]
      , cdVictoryPoints = Just 2
      }
