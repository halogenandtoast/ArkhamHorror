module Arkham.Asset.Cards.RedTideRising where

import Arkham.Asset.Cards.Import

mysteriousPhoto :: CardDef
mysteriousPhoto =
  (storyAsset "90045a" ("Mysterious Photo" <:> "All I Have Left of Him") 0 RedTideRising)
    { cdCardTraits = setFromList [Item]
    , cdCost = Nothing
    , cdDoubleSided = True
    , cdOtherSide = Just "90045b"
    }

mysteriousPhotoBack :: CardDef
mysteriousPhotoBack =
  (storyAsset "90045b" ("Mysterious Photo" <:> "All I Have Left of Him") 0 RedTideRising)
    { cdCardTraits = setFromList [Item]
    , cdCost = Nothing
    , cdDoubleSided = True
    , cdOtherSide = Just "90045a"
    }
