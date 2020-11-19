{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Runner where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait

type EventRunner env
  = ( HasQueue env
    , HasCount env ClueCount InvestigatorId
    , HasCount env ClueCount LocationId
    , HasCount env PlayerCount ()
    , HasId LocationId InvestigatorId env
    , HasList DiscardableHandCard InvestigatorId env
    , HasRoundHistory env
    , HasSet AssetId InvestigatorId env
    , HasSet ConnectedLocationId LocationId env
    , HasSet EmptyLocationId () env
    , HasSet EnemyId InvestigatorId env
    , HasSet EnemyId LocationId env
    , HasSet ExhaustedEnemyId LocationId env
    , HasSet InvestigatorId () env
    , HasSet InvestigatorId LocationId env
    , HasSet RevealedLocationId () env
    , HasSet Trait AssetId env
    , HasSet Trait EnemyId env
    , HasTarget ForSkillTest env
    )
