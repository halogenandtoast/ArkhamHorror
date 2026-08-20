{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.DevilReef where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

deepOnePredator :: CardDef
deepOnePredator =
  (enemy "07182" "Deep One Predator" DevilReef 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = singleton Keyword.Hunter
    }

huntingDeepOne :: CardDef
huntingDeepOne =
  (enemy "07183" "Hunting Deep One" DevilReef 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = singleton Keyword.Hunter
    }

theTerrorOfDevilReef_164 :: CardDef
theTerrorOfDevilReef_164 =
  doubleSided "07164"
    $ (enemy "07164b" "The Terror of Devil Reef" DevilReef 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 6
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

theTerrorOfDevilReef_165 :: CardDef
theTerrorOfDevilReef_165 =
  doubleSided "07165"
    $ (enemy "07165b" "The Terror of Devil Reef" DevilReef 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 6
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      }
