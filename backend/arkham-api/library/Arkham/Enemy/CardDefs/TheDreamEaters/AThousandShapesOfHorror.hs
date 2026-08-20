module Arkham.Enemy.CardDefs.TheDreamEaters.AThousandShapesOfHorror where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

theUnnamable :: CardDef
theUnnamable =
  unique
    $ doubleSided "06169a"
    $ (enemy "06169b" ("The Unnamable" <:> "The Ultimate Abomination") AThousandShapesOfHorror 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      }
