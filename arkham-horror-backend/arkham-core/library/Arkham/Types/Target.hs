module Arkham.Types.Target
  ( Target(..)
  )
where

import Arkham.Types.AssetId
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import ClassyPrelude
import Data.Aeson

data Target
  = AssetTarget AssetId
  | EnemyTarget EnemyId
  | InvestigatorTarget InvestigatorId
  | SkillTestTarget
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
