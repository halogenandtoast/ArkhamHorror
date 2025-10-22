{-# LANGUAGE TemplateHaskell #-}

module Arkham.Entities where

import Arkham.Act
import Arkham.Agenda ()
import Arkham.Agenda.Types (Agenda)
import Arkham.Asset (createAsset)
import Arkham.Asset.Types (Asset)
import Arkham.Campaign ()
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage
import Arkham.Effect ()
import Arkham.Effect.Types (Effect)
import Arkham.Enemy ()
import Arkham.Enemy.Types (Enemy)
import Arkham.Event
import Arkham.Event.Types (Event)
import Arkham.GameT
import Arkham.Id
import Arkham.Investigator ()
import Arkham.Investigator.Types (Investigator)
import Arkham.Json
import Arkham.Location
import Arkham.Placement
import Arkham.Prelude
import Arkham.Scenario ()
import Arkham.Skill (createSkill)
import Arkham.Skill.Types (Skill)
import Arkham.Source
import Arkham.Story
import Arkham.Target
import Arkham.Tracing
import Arkham.Treachery
import Arkham.Treachery.Types (Treachery)
import Arkham.Zone
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Combinators qualified as C
import Data.Map.Strict qualified as Map
import Data.Typeable
import GHC.Records

type EntityMap a = Map (EntityId a) a

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
  , entitiesStories :: EntityMap Story
  }
  deriving stock (Eq, Show, Generic, Data)

instance HasField "investigators" Entities (Map InvestigatorId Investigator) where
  getField = entitiesInvestigators

instance HasField "assets" Entities (Map AssetId Asset) where
  getField = entitiesAssets

instance HasField "enemies" Entities (Map EnemyId Enemy) where
  getField = entitiesEnemies

instance ToJSON Entities where
  toJSON = genericToJSON $ aesonOptions $ Just "entities"

instance FromJSON Entities where
  parseJSON = genericParseJSON $ aesonOptions $ Just "entities"

clearRemovedEntities :: Entities -> Entities
clearRemovedEntities entities =
  entities
    { entitiesEnemies = Map.filter (\e -> e.placement /= OutOfPlay RemovedZone) entities.enemies
    , entitiesAssets = Map.filter (\e -> e.placement /= OutOfPlay RemovedZone) entities.assets
    }

defaultEntities :: Entities
defaultEntities =
  Entities
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
    , entitiesStories = mempty
    }

instance Monoid Entities where
  mempty = defaultEntities

instance Semigroup Entities where
  a <> b =
    Entities
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
      , entitiesStories = entitiesStories a <> entitiesStories b
      }

instance HasAbilities Entities where
  getAbilities Entities {..} =
    concatMap getAbilities (toList entitiesLocations)
      <> concatMap getAbilities (toList entitiesInvestigators)
      <> concatMap getAbilities (toList entitiesEnemies)
      <> concatMap getAbilities (toList entitiesAssets)
      <> concatMap getAbilities (toList entitiesActs)
      <> concatMap getAbilities (toList entitiesAgendas)
      <> concatMap getAbilities (toList entitiesTreacheries)
      <> concatMap getAbilities (toList entitiesEvents)
      <> concatMap getAbilities (toList entitiesEffects)
      <> concatMap getAbilities (toList entitiesSkills)
      <> concatMap getAbilities (toList entitiesStories)

data SomeEntity where
  SomeEntity :: (Entity a, HasModifiersFor a, Targetable a, Sourceable a, Show a) => a -> SomeEntity

deriving stock instance Show SomeEntity

instance Targetable SomeEntity where
  toTarget (SomeEntity e) = toTarget e

instance Sourceable SomeEntity where
  toSource (SomeEntity e) = toSource e

instance HasModifiersFor SomeEntity where
  getModifiersFor (SomeEntity e) = getModifiersFor e

overEntities :: Monoid a => (SomeEntity -> a) -> Entities -> a
overEntities f e = runIdentity $ overEntitiesM (Identity . f) e

overEntitiesM :: (Monoid a, Monad m) => (SomeEntity -> m a) -> Entities -> m a
overEntitiesM f = foldMapM f . toSomeEntities

toSomeEntities :: Entities -> [SomeEntity]
toSomeEntities Entities {..} =
  map SomeEntity (toList entitiesLocations)
    <> map SomeEntity (toList entitiesInvestigators)
    <> map SomeEntity (toList entitiesEnemies)
    <> map SomeEntity (toList entitiesAssets)
    <> map SomeEntity (toList entitiesActs)
    <> map SomeEntity (toList entitiesAgendas)
    <> map SomeEntity (toList entitiesTreacheries)
    <> map SomeEntity (toList entitiesEvents)
    <> map SomeEntity (toList entitiesEffects)
    <> map SomeEntity (toList entitiesSkills)
    <> map SomeEntity (toList entitiesStories)

makeLensesWith suffixedFields ''Entities

-- Entity id generation uses the card id, this is only necessary for entities with non in-play effects
addCardEntityWith
  :: InvestigatorId -> (forall a. Typeable a => a -> a) -> UUID -> Entities -> Card -> Entities
addCardEntityWith i f uuid e card = case card of
  PlayerCard pc -> case toCardType pc of
    EventType ->
      let
        eventId = EventId uuid
        event' = f $ createEvent card i eventId
       in
        e & eventsL %~ insertEntity event'
    AssetType -> do
      let
        assetId = AssetId uuid
        asset = f $ createAsset card assetId
       in
        e & assetsL %~ insertMap (toId asset) asset
    SkillType -> do
      let
        skillId = SkillId uuid
        skill = f $ createSkill card i skillId
       in
        e & skillsL %~ insertMap (toId skill) skill
    PlayerTreacheryType -> do
      let
        treacheryId = TreacheryId uuid
        treachery = f $ createTreachery card i treacheryId
       in
        e & treacheriesL %~ insertMap (toId treachery) treachery
    _ -> error $ "Unable to handle " <> show pc
  EncounterCard ec -> case toCardType ec of
    TreacheryType -> do
      let
        treacheryId = TreacheryId uuid
        treachery = f $ createTreachery card i treacheryId
       in
        e & treacheriesL %~ insertMap (toId treachery) treachery
    _ -> error "Unhandled AddCardEntityWith for encounter card"
  VengeanceCard _ -> error "vengeance card"

addEntity :: forall a. Typeable a => a -> Entities -> Entities
addEntity a e =
  if
    | Just Refl <- eqT @a @Asset -> e & assetsL %~ insertMap (toId a) a
    | otherwise -> e

instance RunMessage Entities where
  runMessage msg entities = withSpan_ "Entities.runMessage" do
    let
      runEntities
        :: (a ~ RunType a, Entity a, RunMessage a, Ord (EntityId a))
        => Lens' Entities (EntityMap a) -> GameT (EntityMap a)
      runEntities lensL =
        runConduit
          $ C.yieldMany (Map.elems $ entities ^. lensL)
          .| C.mapM (runMessage msg)
          .| C.foldMap (\inv -> Map.singleton (toId inv) inv)

    entitiesActs <- runEntities actsL
    entitiesAgendas <- runEntities agendasL
    entitiesTreacheries <- runEntities treacheriesL
    entitiesEvents <- runEntities eventsL
    entitiesLocations <- runEntities locationsL
    entitiesEnemies <- runEntities enemiesL
    entitiesEffects <- runEntities effectsL
    entitiesAssets <- runEntities assetsL
    entitiesSkills <- runEntities skillsL
    entitiesStories <- runEntities storiesL
    entitiesInvestigators <- runEntities investigatorsL
    pure Entities {..}
