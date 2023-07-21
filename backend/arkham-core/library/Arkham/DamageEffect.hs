{-# LANGUAGE TemplateHaskell #-}

module Arkham.DamageEffect where

import Arkham.Prelude

import Arkham.Source
import Data.Aeson.TH

data DamageAssignment = DamageAssignment
  { damageAssignmentSource :: Source
  , damageAssignmentAmount :: Int
  , damageAssignmentDamageEffect :: DamageEffect
  , damageAssignmentDirect :: Bool
  , damageAssignmentDelayed :: Bool
  }
  deriving stock (Show, Eq, Ord)

attack :: Sourceable a => a -> Int -> DamageAssignment
attack a n =
  DamageAssignment
    { damageAssignmentSource = toSource a
    , damageAssignmentAmount = n
    , damageAssignmentDamageEffect = AttackDamageEffect
    , damageAssignmentDirect = False
    , damageAssignmentDelayed = False
    }

nonAttack :: Sourceable a => a -> Int -> DamageAssignment
nonAttack a n =
  DamageAssignment
    { damageAssignmentSource = toSource a
    , damageAssignmentAmount = n
    , damageAssignmentDamageEffect = NonAttackDamageEffect
    , damageAssignmentDirect = False
    , damageAssignmentDelayed = False
    }

storyDamage :: Sourceable a => a -> Int -> DamageAssignment
storyDamage a n =
  DamageAssignment
    { damageAssignmentSource = toSource a
    , damageAssignmentAmount = n
    , damageAssignmentDamageEffect = StoryCardDamageEffect
    , damageAssignmentDirect = False -- modified amount determined
    , damageAssignmentDelayed = False
    }

directDamage :: DamageAssignment -> DamageAssignment
directDamage d = d {damageAssignmentDirect = True}

delayDamage :: DamageAssignment -> DamageAssignment
delayDamage d = d {damageAssignmentDelayed = True}

data DamageEffect
  = AttackDamageEffect
  | NonAttackDamageEffect
  | StoryCardDamageEffect
  deriving stock (Eq, Show, Ord)

$(deriveJSON defaultOptions ''DamageEffect)
$(deriveJSON defaultOptions ''DamageAssignment)
