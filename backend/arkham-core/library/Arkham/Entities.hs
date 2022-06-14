{-# LANGUAGE TemplateHaskell #-}
module Arkham.Entities where

import Arkham.Prelude

import Arkham.Act
import Arkham.Agenda
import Arkham.Asset
import Arkham.Classes.Entity
import Arkham.Classes.RunMessage
import Arkham.Effect
import Arkham.Enemy
import Arkham.Event
import Arkham.Investigator
import Arkham.Location
import Arkham.Skill
import Arkham.Treachery
import Data.Aeson.TH

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

instance RunMessage Entities where
  runMessage msg entities =
    traverseOf (actsL . traverse) (runMessage msg) entities
      >>= traverseOf (agendasL . traverse) (runMessage msg)
      >>= traverseOf (treacheriesL . traverse) (runMessage msg)
      >>= traverseOf (eventsL . traverse) (runMessage msg)
      >>= traverseOf (locationsL . traverse) (runMessage msg)
      >>= traverseOf (enemiesL . traverse) (runMessage msg)
      >>= traverseOf (effectsL . traverse) (runMessage msg)
      >>= traverseOf (assetsL . traverse) (runMessage msg)
      >>= traverseOf (skillsL . traverse) (runMessage msg)
      >>= traverseOf (investigatorsL . traverse) (runMessage msg)

instance Monoid Entities where
  mempty = defaultEntities

instance Semigroup Entities where
  a <> b = Entities
    { entitiesLocations = entitiesLocations a <> entitiesLocations b
    , entitiesInvestigators = entitiesInvestigators a <> entitiesInvestigators b
    , entitiesEnemies = entitiesEnemies a <> entitiesEnemies b
    , entitiesAssets = entitiesAssets a <> entitiesAssets b
    , entitiesActs = entitiesActs a <> entitiesActs b
    , entitiesAgendas = entitiesAgendas a <> entitiesAgendas b
    , entitiesTreacheries = entitiesTreacheries a <> entitiesTreacheries b
    , entitiesEvents = entitiesEvents a <> entitiesEvents b
    , entitiesEffects = entitiesEffects a <> entitiesEffects b
    , entitiesSkills = entitiesSkills a <> entitiesSkills b
    }
