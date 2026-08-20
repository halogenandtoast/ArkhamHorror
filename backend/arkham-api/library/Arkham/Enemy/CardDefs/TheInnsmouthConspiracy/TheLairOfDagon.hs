module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

apostleOfDagon :: CardDef
apostleOfDagon =
  (enemy "07293" "Apostle of Dagon" TheLairOfDagon 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Hybrid, Cultist]
    , cdVictoryPoints = Just 1
    }

cerenerianDeepOne :: CardDef
cerenerianDeepOne =
  (enemy "07294" "Cerenerian Deep One" TheLairOfDagon 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

dagonAwakenedAndEnraged :: CardDef
dagonAwakenedAndEnraged =
  doubleSided "07292"
    $ (enemy "07292b" ("Dagon" <:> "Awakened and Enraged") TheLairOfDagon 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 6
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Massive]
      , cdUnique = True
      , cdVictoryPoints = Just 1
      }

dagonDeepInSlumber :: CardDef
dagonDeepInSlumber =
  doubleSided "07292b"
    $ (enemy "07292" ("Dagon" <:> "Deep in Slumber") TheLairOfDagon 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdUnique = True
      }
