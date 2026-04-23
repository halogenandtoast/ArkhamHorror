module Arkham.EnemyLocation.Cards where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name
import Arkham.Keyword qualified as Keyword
import Arkham.Prelude

enemyLocation :: CardCode -> Name -> CardDef
enemyLocation cardCode name =
  (emptyCardDef cardCode name EnemyLocationCardType)
    { cdDoubleSided = True
    , cdOtherSide = Just $ flippedCardCode cardCode
    , cdLevel = Nothing
    , cdKeywords = setFromList [Keyword.Massive]
    , cdVictoryPoints = Just 0
    }

allEnemyLocationCards :: Map CardCode CardDef
allEnemyLocationCards = mapFrom toCardCode [foyerHemlockHouse]

foyerHemlockHouse :: CardDef
foyerHemlockHouse =
  (enemyLocation "10543b" "Foyer")
    { cdEncounterSet = Just HemlockHouse
    , cdEncounterSetQuantity = Just 1
    }
