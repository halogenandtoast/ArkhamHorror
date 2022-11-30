module Arkham.DamageEffect where

import Arkham.Prelude

import Arkham.Classes.Entity.Source
import Arkham.Source

data DamageAssignment = DamageAssignment
  { damageAssignmentSource :: Source
  , damageAssignmentAmount :: Int
  , damageAssignmentDamageEffect :: DamageEffect
  , damageAssignmentDirect :: Bool
  , damageAssignmentDelayed :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

attack :: SourceEntity a => a -> Int -> DamageAssignment
attack a n = DamageAssignment
  { damageAssignmentSource = toSource a
  , damageAssignmentAmount = n
  , damageAssignmentDamageEffect = AttackDamageEffect
  , damageAssignmentDirect = False
  , damageAssignmentDelayed = False
  }

nonAttack :: SourceEntity a => a -> Int -> DamageAssignment
nonAttack a n = DamageAssignment
  { damageAssignmentSource = toSource a
  , damageAssignmentAmount = n
  , damageAssignmentDamageEffect = NonAttackDamageEffect
  , damageAssignmentDirect = False
  , damageAssignmentDelayed = False
  }

storyDamage :: SourceEntity a => a -> Int -> DamageAssignment
storyDamage a n = DamageAssignment
  { damageAssignmentSource = toSource a
  , damageAssignmentAmount = n
  , damageAssignmentDamageEffect = StoryCardDamageEffect
  , damageAssignmentDirect = False -- modified amount determined
  , damageAssignmentDelayed = False
  }

directDamage :: DamageAssignment -> DamageAssignment
directDamage d = d { damageAssignmentDirect = True }

delayDamage :: DamageAssignment -> DamageAssignment
delayDamage d = d { damageAssignmentDelayed = True }

data DamageEffect
  = AttackDamageEffect
  | NonAttackDamageEffect
  | StoryCardDamageEffect
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
