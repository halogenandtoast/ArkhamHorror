module Arkham.Treachery.Cards.ShadowSpawned
  ( shadowSpawned
  , ShadowSpawned(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Keyword qualified as Keyword
import Arkham.Message
import Arkham.Modifier
import Arkham.Query
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype ShadowSpawned = ShadowSpawned TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowSpawned :: TreacheryCard ShadowSpawned
shadowSpawned = treachery ShadowSpawned Cards.shadowSpawned

instance HasCount ResourceCount env TreacheryId => HasModifiersFor ShadowSpawned where
  getModifiersFor _ (EnemyTarget eid) (ShadowSpawned attrs)
    | treacheryOnEnemy eid attrs = do
      n <- unResourceCount <$> getCount (treacheryId attrs)
      pure $ toModifiers
        attrs
        ([EnemyFight n, HealthModifier n, EnemyEvade n]
        <> [ AddKeyword Keyword.Massive | n >= 3 ]
        )
  getModifiersFor _ _ _ = pure []

instance TreacheryRunner env => RunMessage ShadowSpawned where
  runMessage msg t@(ShadowSpawned attrs) = case msg of
    PlaceEnemyInVoid eid
      | EnemyTarget eid `elem` treacheryAttachedTarget attrs -> pure t
    _ -> ShadowSpawned <$> runMessage msg attrs
