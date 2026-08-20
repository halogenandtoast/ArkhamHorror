module Arkham.Enemy.CardDefs.EdgeOfTheEarth.CityOfTheElderThings where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

benignElderThing :: CardDef
benignElderThing =
  (enemy "08642" "Benign Elder Thing" CityOfTheElderThings 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, ElderThing]
    }

reawakenedElderThing :: CardDef
reawakenedElderThing =
  (enemy "08643" "Reawakened48;46;178;1656;284848;46;178;1656;2848 Elder Thing" CityOfTheElderThings 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

terrorOfTheStarsBaneOfTheElderThings :: CardDef
terrorOfTheStarsBaneOfTheElderThings =
  (enemy "08641" ("Terror of the Stars" <:> "Bane of the Elder Things") CityOfTheElderThings 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }
