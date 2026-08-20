module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.ThePitOfDespair where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

theAmalgam :: CardDef
theAmalgam =
  unique
    $ (enemy "07053" "The Amalgam" ThePitOfDespair 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Monster, Abomination, DeepOne, Elite]
      , cdKeywords = singleton Keyword.Hunter
      }
