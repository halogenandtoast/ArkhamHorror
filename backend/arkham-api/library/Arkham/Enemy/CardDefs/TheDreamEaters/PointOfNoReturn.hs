module Arkham.Enemy.CardDefs.TheDreamEaters.PointOfNoReturn where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

gugSentinel :: CardDef
gugSentinel =
  (enemy "06267" "Gug Sentinel" PointOfNoReturn 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Gug]
    , cdVictoryPoints = Just 1
    , cdTags = [enemyReadyTag]
    }

pitchSpider :: CardDef
pitchSpider =
  (enemy "06273" "Pitch Spider" TerrorOfTheVale 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Spider]
    , cdKeywords = setFromList [Keyword.Swarming (Static 0)]
    }

slitheringDhole :: CardDef
slitheringDhole =
  (enemy "06271" "Slithering Dhole" TerrorOfTheVale 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Dhole, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    }
