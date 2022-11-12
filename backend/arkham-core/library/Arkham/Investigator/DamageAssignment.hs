module Arkham.Investigator.DamageAssignment where

import Arkham.Prelude

import Arkham.Id
import Arkham.Strategy
import Arkham.Target

data DamageAssignment = DamageAssignment
  { damageAssignmentDamage :: Int
  , damageAssignmentHorror :: Int
  , damageAssignmentIsDirect :: Bool
  , damageAssignmentStrategy :: DamageStrategy
  , damageAssignmentHorrorTargets :: [Target]
  , damageAssignmentDamageTargets :: [Target]
  , damageAssignmentInvestigator :: InvestigatorId
  }
