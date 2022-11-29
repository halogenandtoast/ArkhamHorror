module Arkham.Card.Class where

import Arkham.SkillType
import ClassyPrelude

class HasCost a where
  getCost :: a -> Int

class HasSkillIcons a where
  getSkillIcons :: a -> [SkillIcon]
