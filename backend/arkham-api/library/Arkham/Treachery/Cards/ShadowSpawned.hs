module Arkham.Treachery.Cards.ShadowSpawned (shadowSpawned, ShadowSpawned (..)) where

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Prelude
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner
import Arkham.Zone

newtype ShadowSpawned = ShadowSpawned TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowSpawned :: TreacheryCard ShadowSpawned
shadowSpawned = treachery ShadowSpawned Cards.shadowSpawned

instance HasModifiersFor ShadowSpawned where
  getModifiersFor (ShadowSpawned attrs) = case attrs.placement of
    AttachedToEnemy eid -> do
      n <- field TreacheryResources (treacheryId attrs)
      modified_ attrs eid
        $ [EnemyFight n, HealthModifier n, EnemyEvade n]
        <> [AddKeyword Keyword.Massive | n >= 3]
    _ -> pure mempty

instance RunMessage ShadowSpawned where
  runMessage msg t@(ShadowSpawned attrs) = case msg of
    PlaceEnemyOutOfPlay VoidZone eid
      | EnemyTarget eid `elem` treacheryAttachedTarget attrs -> pure t
    _ -> ShadowSpawned <$> runMessage msg attrs
