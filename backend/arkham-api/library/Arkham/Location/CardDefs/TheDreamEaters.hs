{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheDreamEaters where

import Arkham.Location.CardDefs.Import

dreamGateWondrousJourney :: CardDef
dreamGateWondrousJourney =
  (emptyCardDef "06015a" ("Dream-Gate" <:> "Wondrous Journey") LocationType)
    { cdRevealedName = Just $ "Dream-Gate" <:> "Wondrous Journey"
    , cdCardTraits = setFromList [Dreamlands]
    , cdRevealedCardTraits = setFromList [Dreamlands]
    , cdArt = "06015a"
    , cdLocationSymbol = Just NoSymbol
    , cdLocationRevealedSymbol = Just NoSymbol
    , cdLocationConnections = mempty
    , cdLocationRevealedConnections = mempty
    , cdClassSymbols = singleton #neutral
    , cdLevel = Nothing
    , cdOtherSide = Just "06015b"
    }

dreamGatePointlessReality :: CardDef
dreamGatePointlessReality =
  (emptyCardDef "06015b" ("Dream-Gate" <:> "Pointless Reality") LocationType)
    { cdRevealedName = Just $ "Dream-Gate" <:> "Pointless Reality"
    , cdCardTraits = setFromList [Dreamlands]
    , cdRevealedCardTraits = setFromList [Dreamlands]
    , cdDoubleSided = False
    , cdArt = "06015b"
    , cdLocationSymbol = Just NoSymbol
    , cdLocationRevealedSymbol = Just NoSymbol
    , cdLocationConnections = mempty
    , cdLocationRevealedConnections = mempty
    , cdClassSymbols = singleton #neutral
    , cdLevel = Nothing
    }
