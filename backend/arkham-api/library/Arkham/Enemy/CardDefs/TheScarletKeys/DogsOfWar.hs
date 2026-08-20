module Arkham.Enemy.CardDefs.TheScarletKeys.DogsOfWar where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

coterieProvocateur :: CardDef
coterieProvocateur =
  (enemy "09657" "Coterie Provocateur" DogsOfWar 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Coterie]
    , cdKeywords =
        setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "keyLocus"))]
    }

scarletBeast :: CardDef
scarletBeast =
  (enemy "09656" "Scarlet Beast" DogsOfWar 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Coterie]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
    }

theBeastInACowlOfCrimsonRavagerInRed :: CardDef
theBeastInACowlOfCrimsonRavagerInRed =
  (enemy "09655" ("The Beast in a Cowl of Crimson" <:> "Ravager in Red") DogsOfWar 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords =
        setFromList [Keyword.Patrol (LocationWithModifier (ScenarioModifier "keyLocus")), Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09655b"
    }

theBeastInACowlOfCrimsonWolfInSheepsClothing :: CardDef
theBeastInACowlOfCrimsonWolfInSheepsClothing =
  (enemy "09655b" ("The Beast in a Cowl of Crimson" <:> "Wolf in Sheep's Clothing") DogsOfWar 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 5
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09655"
    }

theClaretKnightCoterieKingpin :: CardDef
theClaretKnightCoterieKingpin =
  (enemy "09654" ("The Claret Knight" <:> "Coterie Kingping") DogsOfWar 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09654b"
    }
