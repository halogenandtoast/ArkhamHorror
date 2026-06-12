module Arkham.Asset.Cards.RelicsOfThePast where

import Arkham.Asset.Cards.Import

jadeCrocodile :: CardDef
jadeCrocodile =
  (storyAsset_ "90073" ("Jade Crocodile" <:> "Effigy of the Guardian") RelicsOfThePast)
    { cdCardTraits = setFromList [Item, Relic, Ancient]
    , cdUnique = True
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    , cdVictoryPoints = Just 1
    }

obsidianJaguar :: CardDef
obsidianJaguar =
  (storyAsset_ "90074" ("Obsidian Jaguar" <:> "Effigy of the Huntress") RelicsOfThePast)
    { cdCardTraits = setFromList [Item, Relic, Ancient]
    , cdUnique = True
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    , cdVictoryPoints = Just 1
    }

citrineSnake :: CardDef
citrineSnake =
  (storyAsset_ "90075" ("Citrine Snake" <:> "Effigy of the Child") RelicsOfThePast)
    { cdCardTraits = setFromList [Item, Relic, Ancient]
    , cdUnique = True
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    , cdVictoryPoints = Just 1
    }

turquoiseEagle :: CardDef
turquoiseEagle =
  (storyAsset_ "90076" ("Turquoise Eagle" <:> "Effigy of the Watcher") RelicsOfThePast)
    { cdCardTraits = setFromList [Item, Relic, Ancient]
    , cdUnique = True
    , cdRevelation = IsRevelation
    , cdCardType = EncounterAssetType
    , cdVictoryPoints = Just 1
    }
