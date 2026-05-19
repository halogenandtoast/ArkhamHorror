module Arkham.Location.CardDefs.Base where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet hiding (Arkham, Blight, Dreamlands, Dunwich)
import Arkham.Keyword qualified as Keyword
import Arkham.LocationSymbol
import Arkham.Name
import Arkham.Prelude
import Arkham.Trait hiding (Circle)
import Data.Set qualified as Set

locationWithUnrevealed
  :: CardCode
  -> Name
  -> [Trait]
  -> LocationSymbol
  -> [LocationSymbol]
  -> Name
  -> [Trait]
  -> LocationSymbol
  -> [LocationSymbol]
  -> EncounterSet
  -> CardDef
locationWithUnrevealed cardCode unrevealedName unrevealedTraits unrevealedLocationSymbol unrevealedConnectedLocationSymbols name traits locationSymbol connectedLocationSymbols encounterSet =
  ( location
      cardCode
      unrevealedName
      unrevealedTraits
      unrevealedLocationSymbol
      unrevealedConnectedLocationSymbols
      encounterSet
  )
    { cdRevealedName = Just name
    , cdRevealedCardTraits = setFromList traits
    , cdLocationRevealedSymbol = Just locationSymbol
    , cdLocationRevealedConnections = connectedLocationSymbols
    , cdLevel = Nothing
    }

locationWithUnrevealed_
  :: CardCode
  -> Name
  -> [Trait]
  -> Name
  -> [Trait]
  -> EncounterSet
  -> CardDef
locationWithUnrevealed_ cardCode unrevealedName unrevealedTraits name traits encounterSet =
  ( location
      cardCode
      unrevealedName
      unrevealedTraits
      NoSymbol
      []
      encounterSet
  )
    { cdRevealedName = Just name
    , cdRevealedCardTraits = setFromList traits
    , cdLevel = Nothing
    }

location
  :: CardCode
  -> Name
  -> [Trait]
  -> LocationSymbol
  -> [LocationSymbol]
  -> EncounterSet
  -> CardDef
location cardCode name traits locationSymbol connectedLocationSymbols encounterSet =
  (emptyCardDef cardCode name LocationType)
    { cdRevealedName = Just name
    , cdCardTraits = setFromList traits
    , cdRevealedCardTraits = setFromList traits
    , cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just 1
    , cdDoubleSided = True
    , cdLocationSymbol = Just locationSymbol
    , cdLocationRevealedSymbol = Just locationSymbol
    , cdLocationConnections = connectedLocationSymbols
    , cdLocationRevealedConnections = connectedLocationSymbols
    , cdLevel = Nothing
    }

location_
  :: CardCode
  -> Name
  -> [Trait]
  -> EncounterSet
  -> CardDef
location_ cardCode name traits encounterSet = location cardCode name traits NoSymbol mempty encounterSet

withMeta :: ToJSON a => (Text, a) -> CardDef -> CardDef
withMeta (k, v) def = def {cdMeta = insertMap k (toJSON v) def.meta}

vengeance :: Int -> CardDef -> CardDef
vengeance n def = def {cdVengeancePoints = Just n}

victory :: Int -> CardDef -> CardDef
victory n def = def {cdVictoryPoints = Just n}

revelation :: CardDef -> CardDef
revelation def = def {cdRevelation = IsRevelation}

singleSided :: CardDef -> CardDef
singleSided def = def {cdDoubleSided = False}

otherSideIs :: CardCode -> CardDef -> CardDef
otherSideIs ccode def =
  def
    { cdDoubleSided = False
    , cdOtherSide = Just ccode
    }

veiled :: CardDef -> CardDef
veiled def =
  def
    { cdDoubleSided = False
    , cdKeywords = Set.insert Keyword.Veiled $ cdKeywords def
    , cdOtherSide = Just (flippedCardCode def.cardCode)
    }

storyOnBack :: CardDef -> CardDef
storyOnBack def = def {cdDoubleSided = False, cdOtherSide = Just (flippedCardCode def.cardCode)}

storyOnBack' :: CardCode -> CardDef -> CardDef
storyOnBack' back def = def {cdDoubleSided = False, cdOtherSide = Just back}

quantity :: Int -> CardDef -> CardDef
quantity n def = def {cdEncounterSetQuantity = Just n}
