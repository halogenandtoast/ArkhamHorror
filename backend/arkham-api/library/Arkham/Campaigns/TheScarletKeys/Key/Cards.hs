module Arkham.Campaigns.TheScarletKeys.Key.Cards where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name
import Arkham.Prelude

key :: CardCode -> Name -> EncounterSet -> CardDef
key cardCode name encounterSet =
  (emptyCardDef cardCode name KeyType)
    { cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just 1
    , cdDoubleSided = True
    , cdOtherSide = Just $ flippedCardCode cardCode
    , cdLevel = Nothing
    }

allScarletKeyCards :: Map CardCode CardDef
allScarletKeyCards = mapFromList $ map (toCardCode &&& id) [theEyeOfRavens]

theEyeOfRavens :: CardDef
theEyeOfRavens = key "09519" ("The Eye of Ravens" <:> "Macabre Ruby Marble") RiddlesAndRain
