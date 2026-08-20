module Arkham.Enemy.CardDefs.ThePathToCarcosa.TheUnspeakableOath where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

asylumGorger :: CardDef
asylumGorger =
  (enemy "03183" "Asylum Gorger" TheUnspeakableOath 2)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 3
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = singleton Keyword.Hunter
    }

danielChesterfield :: CardDef
danielChesterfield =
  unique
    $ doubleSided "03182a"
    $ ( enemy
          "03182b"
          ("Daniel Chesterfield" <:> "…Or At Least, What's Left of Him")
          TheUnspeakableOath
          1
      )
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Lunatic, Elite]
      , cdVictoryPoints = Just 1
      }

madPatient :: CardDef
madPatient =
  (enemy "03184" "Mad Patient" TheUnspeakableOath 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Lunatic]
    }
