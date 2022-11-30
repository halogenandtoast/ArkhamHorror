module Arkham.DamageEffect where

import Arkham.Prelude

import Arkham.Classes.Entity.Source
import Arkham.Source

data DamageAssignment = DamageAssignment
  { damageAssignmentSource :: Source
  , damageAssignmentAmount :: Int
  , damageAssignmentDamageEffect :: DamageEffect
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

attack :: SourceEntity a => a -> Int -> DamageAssignment
attack a n = DamageAssignment
  { damageAssignmentSource = toSource a
  , damageAssignmentAmount = n
  , damageAssignmentDamageEffect = AttackDamageEffect
  }

nonAttack :: SourceEntity a => a -> Int -> DamageAssignment
nonAttack a n = DamageAssignment
  { damageAssignmentSource = toSource a
  , damageAssignmentAmount = n
  , damageAssignmentDamageEffect = NonAttackDamageEffect
  }

storyDamage :: SourceEntity a => a -> Int -> DamageAssignment
storyDamage a n = DamageAssignment
  { damageAssignmentSource = toSource a
  , damageAssignmentAmount = n
  , damageAssignmentDamageEffect = StoryCardDamageEffect
  }

data DamageEffect
  = AttackDamageEffect
  | NonAttackDamageEffect
  | StoryCardDamageEffect
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
