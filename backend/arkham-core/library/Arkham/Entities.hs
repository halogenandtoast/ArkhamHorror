{-# LANGUAGE TemplateHaskell #-}
module Arkham.Entities where

import Arkham.Prelude

import Arkham.Act
import Arkham.Agenda ()
import Arkham.Agenda.Types (Agenda)
import Arkham.Asset (createAsset)
import Arkham.Asset.Types (Asset)
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.RunMessage
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Effect ()
import Arkham.Effect.Types (Effect)
import Arkham.Enemy ()
import Arkham.Enemy.Types (Enemy)
import Arkham.Event
import Arkham.Event.Types (Event)
import Arkham.Id
import Arkham.Investigator ()
import Arkham.Investigator.Types (Investigator)
import Arkham.Json
import Arkham.Location
import Arkham.Skill ()
import Arkham.Skill.Types (Skill)
import Arkham.Target
import Arkham.Treachery
import Arkham.Treachery.Types (Treachery)

-- Entity id generation uses the card id, thi sis only necessary for entities with non in-play effects
addCardEntityWith :: MonadRandom m => Investigator -> (forall a. Typeable a => a -> a) -> Entities -> Card -> m Entities
addCardEntityWith i f e card = case card of
  PlayerCard pc -> case toCardType pc of
    EventType -> do
      let
        eventId = EventId uuid
        event' = createEvent card (toId i) eventId
      pure $ e & eventsL %~ insertEntity event'
    AssetType -> do
      let
        assetId = AssetId uuid
        asset = f $ createAsset card assetId
      pure $ e & assetsL %~ insertMap (toId asset) asset
    _ -> error "Unhandled"
  EncounterCard ec -> case toCardType ec of
    TreacheryType -> do
      let
        treacheryId = TreacheryId uuid
        treachery = createTreachery card (toId i) treacheryId
      pure $ e & treacheriesL %~ insertMap (toId treachery) treachery
    _ -> error "Unhandled"
  VengeanceCard _ -> error "vengeance card"
 where
  uuid = unsafeCardIdToUUID (toCardId card)

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
  deriving stock (Eq, Show, Generic)

instance ToJSON Entities where
  toJSON = genericToJSON $ aesonOptions $ Just "entities"

instance FromJSON Entities where
  parseJSON = genericParseJSON $ aesonOptions $ Just "entities"

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

data SomeEntity where
  SomeEntity :: (Show e, Targetable e, Entity e, HasModifiersFor e) => e -> SomeEntity

instance Targetable SomeEntity where
  toTarget (SomeEntity e) = toTarget e

deriving stock instance Show SomeEntity

instance HasModifiersFor SomeEntity where
  getModifiersFor target (SomeEntity e) = getModifiersFor target e

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

makeLensesWith suffixedFields ''Entities
