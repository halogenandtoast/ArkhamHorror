{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Runner where

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId

type SkillRunner env
  = ( HasQueue env
    , HasSet ConnectedLocationId LocationId env
    , HasSet BlockedLocationId () env
    , HasSet EnemyId InvestigatorId env
    , HasId LocationId InvestigatorId env
    )
