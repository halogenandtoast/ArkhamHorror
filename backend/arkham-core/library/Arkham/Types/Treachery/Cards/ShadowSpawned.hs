module Arkham.Types.Treachery.Cards.ShadowSpawned
  ( shadowSpawned
  , ShadowSpawned(..)
  )
where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Game.Helpers
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ShadowSpawned = ShadowSpawned TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowSpawned :: TreacheryId -> a -> ShadowSpawned
shadowSpawned uuid _ = ShadowSpawned $ baseAttrs uuid "02142"

instance HasCount ResourceCount env TreacheryId => HasModifiersFor env ShadowSpawned where
  getModifiersFor _ (EnemyTarget eid) (ShadowSpawned attrs)
    | treacheryOnEnemy eid attrs = do
      n <- unResourceCount <$> getCount (treacheryId attrs)
      pure $ toModifiers
        attrs
        ([EnemyFight n, HealthModifier n, EnemyEvade n]
        <> [ AddKeyword Keyword.Massive | n >= 3 ]
        )
  getModifiersFor _ _ _ = pure []

instance HasActions env ShadowSpawned where
  getActions i window (ShadowSpawned attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env ShadowSpawned where
  runMessage msg t@(ShadowSpawned attrs) = case msg of
    PlaceEnemyInVoid eid
      | EnemyTarget eid `elem` treacheryAttachedTarget attrs -> pure t
    _ -> ShadowSpawned <$> runMessage msg attrs
