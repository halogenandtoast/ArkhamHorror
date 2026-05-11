module Arkham.EnemyLocation.Cards where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.EncounterSet
import Arkham.Name
import Arkham.Keyword qualified as Keyword
import Arkham.Prelude
import Arkham.Trait (Trait (..))

enemyLocation :: CardCode -> Name -> CardDef
enemyLocation cardCode name =
  (emptyCardDef cardCode name EnemyLocationCardType)
    { cdDoubleSided = True
    , cdOtherSide = Just $ flippedCardCode cardCode
    , cdLevel = Nothing
    , cdKeywords = setFromList [Keyword.Massive]
    , cdCardTraits = setFromList [Room, Monster, Elite]
    , cdRevealedCardTraits = setFromList [Room, Monster, Elite]
    , cdVictoryPoints = Just 0
    , cdEncounterSet = Just HemlockHouse
    , cdEncounterSetQuantity = Just 1
    }

-- | Shapeless Cellar is single-sided (no Dormant location side); it cannot be
-- sealed, cannot be flipped, and does not have a paired Dormant location card.
shapelessCellarDef :: CardDef
shapelessCellarDef =
  (emptyCardDef "10547" "Shapeless Cellar" EnemyLocationCardType)
    { cdDoubleSided = False
    , cdLevel = Nothing
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
    , cdCardTraits = setFromList [Room, Monster, Elite]
    , cdRevealedCardTraits = setFromList [Room, Monster, Elite]
    , cdVictoryPoints = Just 0
    , cdEncounterSet = Just HemlockHouse
    , cdEncounterSetQuantity = Just 1
    }

allEnemyLocationCards :: Map CardCode CardDef
allEnemyLocationCards =
  mapFrom toCardCode
    [ livingBedroomHemlockHouse32
    , livingBedroomHemlockHouse33
    , livingBedroomHemlockHouse34
    , livingBedroomHemlockHouse35
    , livingWashroomHemlockHouse36
    , livingWashroomHemlockHouse37
    , livingWashroomHemlockHouse38
    , livingLibraryHemlockHouse39
    , livingLibraryHemlockHouse40
    , livingParlorHemlockHouse
    , livingDiningRoomHemlockHouse
    , foyerHemlockHouse
    , shapelessCellar
    ]

livingBedroomHemlockHouse32 :: CardDef
livingBedroomHemlockHouse32 = enemyLocation "10532b" "Living Bedroom"

livingBedroomHemlockHouse33 :: CardDef
livingBedroomHemlockHouse33 = enemyLocation "10533b" "Living Bedroom"

livingBedroomHemlockHouse34 :: CardDef
livingBedroomHemlockHouse34 = enemyLocation "10534b" "Living Bedroom"

livingBedroomHemlockHouse35 :: CardDef
livingBedroomHemlockHouse35 = enemyLocation "10535b" "Living Bedroom"

livingWashroomHemlockHouse36 :: CardDef
livingWashroomHemlockHouse36 = enemyLocation "10536b" "Living Washroom"

livingWashroomHemlockHouse37 :: CardDef
livingWashroomHemlockHouse37 = enemyLocation "10537b" "Living Washroom"

livingWashroomHemlockHouse38 :: CardDef
livingWashroomHemlockHouse38 = enemyLocation "10538b" "Living Washroom"

livingLibraryHemlockHouse39 :: CardDef
livingLibraryHemlockHouse39 = enemyLocation "10539b" "Living Library"

livingLibraryHemlockHouse40 :: CardDef
livingLibraryHemlockHouse40 = enemyLocation "10540b" "Living Library"

livingParlorHemlockHouse :: CardDef
livingParlorHemlockHouse = enemyLocation "10541b" "Living Parlor"

livingDiningRoomHemlockHouse :: CardDef
livingDiningRoomHemlockHouse = enemyLocation "10542b" "Living Dining Room"

foyerHemlockHouse :: CardDef
foyerHemlockHouse = enemyLocation "10543b" "Living Foyer"

shapelessCellar :: CardDef
shapelessCellar = shapelessCellarDef
