module Arkham.Types.Card.Class where

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Id
import Arkham.Types.SkillType
import ClassyPrelude

class HasCardCode a where
  getCardCode :: a -> CardCode

class HasCost a where
  getCost :: a -> Int

class HasCardId a where
  getCardId :: a -> CardId

class HasSkillIcons a where
  getSkillIcons :: a -> [SkillType]
