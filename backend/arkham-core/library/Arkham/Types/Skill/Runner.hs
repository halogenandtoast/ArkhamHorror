{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Runner where

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId

type SkillRunner env
  = ( HasQueue env
    , HasSet ConnectedLocationId env LocationId
    , HasSet BlockedLocationId env ()
    , HasSet EnemyId env InvestigatorId
    , HasId LocationId InvestigatorId env
    )
