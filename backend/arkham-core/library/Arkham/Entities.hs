{-# LANGUAGE TemplateHaskell #-}
module Arkham.Entities where

import Arkham.Prelude

import Arkham.Act
import Arkham.Agenda
import Arkham.Asset
import Arkham.Effect
import Arkham.Enemy
import Arkham.Event
import Arkham.Investigator
import Arkham.Location
import Arkham.Skill
import Arkham.Treachery

type EntityMap a = HashMap (EntityId a) a

data Entities = Entities
  { entitiesLocations :: EntityMap Location
  , entitiesInvestigators :: EntityMap Investigator
  , entitiesEnemies :: EntityMap Enemy
  , entitiesAssets :: EntityMap Asset
  , entitiesActs :: EntityMap Act
  , entitiesAgendas :: EntityMap Agenda
  , entitiesTreacheries :: EntityMap Treachery
  , entitiesEvents :: EntityMap Event
  , entitiesEffects :: EntityMap Effect
  , entitiesSkills :: EntityMap Skill
  }
  deriving stock (Eq, Show)

$(deriveJSON defaultOptions ''Entities)

makeLensesWith suffixedFields ''Entities

defaultEntities :: Entities
defaultEntities = Entities
  { entitiesLocations = mempty
  , entitiesInvestigators = mempty
  , entitiesEnemies = mempty
  , entitiesAssets = mempty
  , entitiesActs = mempty
  , entitiesAgendas = mempty
  , entitiesTreacheries = mempty
  , entitiesEvents = mempty
  , entitiesEffects = mempty
  , entitiesSkills = mempty
  }


