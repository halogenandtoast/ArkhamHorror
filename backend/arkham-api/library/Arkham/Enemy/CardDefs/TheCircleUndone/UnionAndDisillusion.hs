module Arkham.Enemy.CardDefs.TheCircleUndone.UnionAndDisillusion where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

gavriellaMizrah :: CardDef
gavriellaMizrah =
  unique
    $ doubleSided "05262"
    $ (enemy "05262b" ("Gavriella Mizrah" <:> "You're Next") UnionAndDisillusion 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 5
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

jeromeDavids :: CardDef
jeromeDavids =
  unique
    $ doubleSided "05263"
    $ (enemy "05263b" ("Jerome Davids" <:> "Starved for Answers") UnionAndDisillusion 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

pennyWhite :: CardDef
pennyWhite =
  unique
    $ doubleSided "05264"
    $ (enemy "05264b" ("Penny White" <:> "Tragic Loss") UnionAndDisillusion 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

spectralRaven :: CardDef
spectralRaven =
  (enemy "05267" "Spectral Raven" UnionAndDisillusion 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Spectral]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
    }

valentinoRivas :: CardDef
valentinoRivas =
  unique
    $ doubleSided "05265"
    $ (enemy "05265b" ("Valentino Rivas" <:> "Ripped Asunder") UnionAndDisillusion 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Geist, Spectral, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }

whippoorwill :: CardDef
whippoorwill =
  (enemy "05266" "Whippoorwill" UnionAndDisillusion 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }
