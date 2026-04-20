module Arkham.EnemyLocation.Cards where

import Arkham.Prelude
import Arkham.Card.CardDef

allEnemyLocationCards :: Map CardCode CardDef
allEnemyLocationCards = mapFrom toCardCode []
