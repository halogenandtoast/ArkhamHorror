module Arkham.EnemyLocation.Cards where

import Arkham.Prelude
import Arkham.Card.CardCode
import Arkham.Card.CardDef

allEnemyLocationCards :: Map CardCode CardDef
allEnemyLocationCards = mapFrom toCardCode []
