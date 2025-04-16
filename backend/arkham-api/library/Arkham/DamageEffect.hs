{-# LANGUAGE TemplateHaskell #-}

module Arkham.DamageEffect where

import Arkham.Id
import Arkham.Prelude
import Arkham.Source
import Data.Aeson.TH
import GHC.Records

data DamageAssignment = DamageAssignment
  { damageAssignmentSource :: Source
  , damageAssignmentAmount :: Int
  , damageAssignmentDamageEffect :: DamageEffect
  , damageAssignmentDirect :: Bool
  , damageAssignmentDelayed :: Bool
  }
  deriving stock (Show, Eq, Ord, Data)

instance HasField "source" DamageAssignment Source where
  getField = damageAssignmentSource

instance HasField "amount" DamageAssignment Int where
  getField = damageAssignmentAmount

attack :: Sourceable a => a -> Int -> DamageAssignment
attack a n =
  DamageAssignment
    { damageAssignmentSource = toSource a
    , damageAssignmentAmount = n
    , damageAssignmentDamageEffect = AttackDamageEffect
    , damageAssignmentDirect = False
    , damageAssignmentDelayed = False
    }

nonAttack :: (Sourceable a) => Maybe InvestigatorId -> a -> Int -> DamageAssignment
nonAttack mInvestigator a n =
  DamageAssignment
    { damageAssignmentSource = maybe (toSource a) (`wrapAbilityUse` toSource a) mInvestigator
    , damageAssignmentAmount = n
    , damageAssignmentDamageEffect = NonAttackDamageEffect
    , damageAssignmentDirect = False
    , damageAssignmentDelayed = False
    }
  where
    wrapAbilityUse iid = \case
      AbilitySource source idx -> UseAbilitySource iid source idx
      other -> other

storyDamage :: Sourceable a => a -> Int -> DamageAssignment
storyDamage a n =
  DamageAssignment
    { damageAssignmentSource = toSource a
    , damageAssignmentAmount = n
    , damageAssignmentDamageEffect = StoryCardDamageEffect
    , damageAssignmentDirect = False -- modified amount determined
    , damageAssignmentDelayed = False
    }

isDirect :: DamageAssignment -> DamageAssignment
isDirect d = d {damageAssignmentDirect = True}

delayDamage :: DamageAssignment -> DamageAssignment
delayDamage d = d {damageAssignmentDelayed = True}

data DamageEffect
  = AttackDamageEffect
  | NonAttackDamageEffect
  | StoryCardDamageEffect
  deriving stock (Eq, Show, Ord, Data)

$(deriveJSON defaultOptions ''DamageEffect)
$(deriveJSON defaultOptions ''DamageAssignment)

