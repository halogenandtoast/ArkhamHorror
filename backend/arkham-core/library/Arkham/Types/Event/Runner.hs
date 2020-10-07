{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Runner where

import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait

type EventRunner env
  = ( HasQueue env
    , HasId LocationId InvestigatorId env
    , HasCount ClueCount LocationId env
    , HasSet InvestigatorId LocationId env
    , HasSet EnemyId LocationId env
    , HasSet ConnectedLocationId LocationId env
    , HasSet EnemyId InvestigatorId env
    , HasSet EmptyLocationId () env
    , HasSet RevealedLocationId () env
    , HasSet ExhaustedEnemyId LocationId env
    , HasSet Trait EnemyId env
    , HasSet Trait AssetId env
    , HasSet AssetId InvestigatorId env
    , HasCount ClueCount InvestigatorId env
    , HasTarget ForSkillTest env
    )
