module Arkham.Types.Card.Class where

import Arkham.Types.SkillType
import ClassyPrelude

class HasCost a where
  getCost :: a -> Int

class HasSkillIcons a where
  getSkillIcons :: a -> [SkillType]
