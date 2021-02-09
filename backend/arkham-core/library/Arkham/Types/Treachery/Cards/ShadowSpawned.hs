module Arkham.Types.Treachery.Cards.ShadowSpawned
  ( shadowSpawned
  , ShadowSpawned(..)
  )
where


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
