module Arkham.Enemy.CardDefs.MachinationsThroughTime where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

edwinBennetBitterAdversary :: CardDef
edwinBennetBitterAdversary =
  unique
    $ (enemy "87036" ("Edwin Bennet" <:> "Bitter Adversary") MachinationsThroughTimeSingleGroup 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords =
          setFromList [Keyword.Patrol (LocationWithAsset $ AssetWithTrait Scientist)]
      , cdDoubleSided = True
      , cdOtherSide = Just "87036b"
      , cdArt = "87036"
      }

ghastlySatyr :: CardDef
ghastlySatyr =
  (enemy "87044" "Ghastly Satyr" MachinationsThroughTime 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords =
        setFromList [Keyword.Patrol (LocationWithAsset $ AssetWithTrait Scientist)]
    }

houndOfTindalos :: CardDef
houndOfTindalos =
  (enemy "87045" "Hound of Tindalos" MachinationsThroughTime 4)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords =
        setFromList [Keyword.Patrol (LocationWithAsset $ AssetWithTrait Scientist)]
    }

manyAngledThing :: CardDef
manyAngledThing =
  (enemy "87046" "Many-Angled Thing" MachinationsThroughTime 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 7
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

oldSadieSheldon :: CardDef
oldSadieSheldon =
  unique
    $ (enemy "87040" ("Old Sadie Sheldon" <:> "Bootlegging Kingpin") MachinationsThroughTime 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 1
      , cdEvade = evade 1
      , cdHealth = health 1
      , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
      , cdKeywords = setFromList [Keyword.Aloof]
      , cdVictoryPoints = Just 0
      }

sheldonGang :: CardDef
sheldonGang =
  (enemy "87041" "Sheldon Gang" MachinationsThroughTime 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate, Elite]
    , cdKeywords = setFromList [Keyword.Surge, Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 0
    }

tindalosAlpha :: CardDef
tindalosAlpha =
  (enemy "87047" "Tindalos Alpha" MachinationsThroughTime 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
    }

tyrthrha :: CardDef
tyrthrha =
  unique
    $ (enemy "87043" ("Tyr'thrha" <:> "Arch-Lord of Tindalos") MachinationsThroughTime 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthStar
      , cdCardTraits = setFromList [AncientOne, Tindalos, Elite]
      , cdKeywords =
          setFromList
            [ Keyword.Massive
            , Keyword.Retaliate
            , Keyword.Patrol (LocationWithAsset $ AssetWithTrait Scientist)
            ]
      , cdVictoryPoints = Just 0
      }
