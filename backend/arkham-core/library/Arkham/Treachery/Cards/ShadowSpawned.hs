module Arkham.Treachery.Cards.ShadowSpawned (
  shadowSpawned,
  ShadowSpawned (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ShadowSpawned = ShadowSpawned TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

shadowSpawned :: TreacheryCard ShadowSpawned
shadowSpawned = treachery ShadowSpawned Cards.shadowSpawned

instance HasModifiersFor ShadowSpawned where
  getModifiersFor (EnemyTarget eid) (ShadowSpawned attrs)
    | treacheryOnEnemy eid attrs = do
        n <- field TreacheryResources (treacheryId attrs)
        pure
          $ toModifiers
            attrs
            ( [EnemyFight n, HealthModifier n, EnemyEvade n]
                <> [AddKeyword Keyword.Massive | n >= 3]
            )
  getModifiersFor _ _ = pure []

instance RunMessage ShadowSpawned where
  runMessage msg t@(ShadowSpawned attrs) = case msg of
    PlaceEnemyInVoid eid
      | EnemyTarget eid `elem` treacheryAttachedTarget attrs -> pure t
    _ -> ShadowSpawned <$> runMessage msg attrs
