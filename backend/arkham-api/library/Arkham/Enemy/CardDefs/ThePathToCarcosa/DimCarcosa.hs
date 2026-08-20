module Arkham.Enemy.CardDefs.ThePathToCarcosa.DimCarcosa where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

creatureOutOfDemhe :: CardDef
creatureOutOfDemhe =
  (enemy "03335" "Creature Out of Demhe" DimCarcosa 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Massive
    }

hasturLordOfCarcosa :: CardDef
hasturLordOfCarcosa =
  unique
    $ (enemy "03333" ("Hastur" <:> "Lord of Carcosa") DimCarcosa 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 9
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      }

hasturTheKingInYellow :: CardDef
hasturTheKingInYellow =
  unique
    $ (enemy "03332" ("Hastur" <:> "The King in Yellow") DimCarcosa 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 7
      , cdCardTraits = setFromList [AncientOne, Elite]
      }

hasturTheTatteredKing :: CardDef
hasturTheTatteredKing =
  unique
    $ (enemy "03334" ("Hastur" <:> "The Tattered King") DimCarcosa 1)
      { cdSanityDamage = sanityDamage 4
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 8
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = singleton Keyword.Hunter
      }

wingedOne :: CardDef
wingedOne =
  (enemy "03336" "Winged One" DimCarcosa 1)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = singleton Keyword.Retaliate
    }
