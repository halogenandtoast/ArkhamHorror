module Arkham.Enemy.CardDefs.TheDrownedCity.TheApiary where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

apiaryTender :: CardDef
apiaryTender =
  (enemy "11575" "Apiary Tender" TheApiary 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Stowaway]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

grotesqueAmalgam :: CardDef
grotesqueAmalgam =
  (enemy "11574" "Grotesque Amalgam" TheApiary 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Stowaway]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

mother :: CardDef
mother =
  unique
    $ (enemy "11573" "Mother" TheApiary 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 1
      , cdHealth = healthPerInvestigator 8
      , cdCardTraits = setFromList [Abomination, Stowaway, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Relentless]
      , cdVictoryPoints = Just 2
      }

squamousParasite :: CardDef
squamousParasite =
  doubleSided "11580b"
    $ (enemy "11580" "Squamous Parasite" TheApiary 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 1
      , cdCardTraits = setFromList [Monster, Glyph]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }
