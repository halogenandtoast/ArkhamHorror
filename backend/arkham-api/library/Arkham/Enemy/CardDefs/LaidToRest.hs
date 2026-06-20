{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.LaidToRest where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

jeanDevereuxSeekingClosure :: CardDef
jeanDevereuxSeekingClosure =
  unique
    $ doubleSided "90057b"
    $ (enemy "90057a" ("Jean Devereux" <:> "Seeking Closure") LaidToRest 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
      , cdKeywords =
          setFromList
            [ Keyword.Aloof
            , Keyword.Patrol (LocationWithCardsUnderneath $ HasCard $ CardWithTitle "Ravenous Spirit")
            ]
      }

jeanDevereuxPossessed :: CardDef
jeanDevereuxPossessed =
  unique
    $ doubleSided "90057a"
    $ (enemy "90057b" ("Jean Devereux" <:> "Possessed") LaidToRest 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      }

ravenousSpirit :: CardDef
ravenousSpirit =
  (enemy "90058" ("Ravenous Spirit" <:> "Possessed") LaidToRest 4)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Geist, Spectral, Elite]
    , cdKeywords =
        setFromList
          [ Keyword.Aloof
          , Keyword.Patrol (LocationWithEnemy $ EnemyWithTitle "Jean Devereux")
          , Keyword.Retaliate
          ]
    , cdVictoryPoints = Just 1
    }
