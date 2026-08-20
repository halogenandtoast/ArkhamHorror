module Arkham.Enemy.CardDefs.TheDreamEaters.AgentsOfNyarlathotep where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

theCrawlingMist :: CardDef
theCrawlingMist =
  (enemy "06086" "The Crawling Mist" AgentsOfNyarlathotep 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Avatar]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }
