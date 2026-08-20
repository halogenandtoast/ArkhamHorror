module Arkham.Enemy.CardDefs.MachinationsThroughTimeEpicMultiplayer where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

edwinBennetEnviousRival :: CardDef
edwinBennetEnviousRival =
  unique
    $ (enemy "87037" ("Edwin Bennet" <:> "Envious Rival") MachinationsThroughTimeEpicMultiplayer 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords =
          setFromList [Keyword.Patrol (LocationWithAsset $ AssetWithTrait Scientist)]
      , cdDoubleSided = True
      , cdOtherSide = Just "87037b"
      , cdArt = "87037"
      }
