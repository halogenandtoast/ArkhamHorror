module Arkham.Treachery.Cards.ShadowSpawned (shadowSpawned) where

import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Keyword qualified as Keyword
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Zone

newtype ShadowSpawned = ShadowSpawned TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowSpawned :: TreacheryCard ShadowSpawned
shadowSpawned = treachery ShadowSpawned Cards.shadowSpawned

instance HasModifiersFor ShadowSpawned where
  getModifiersFor (ShadowSpawned attrs) = case attrs.placement of
    AttachedToEnemy eid -> do
      n <- field TreacheryResources attrs.id
      modified_ attrs eid
        $ [EnemyFight n, HealthModifier n, EnemyEvade n]
        <> [AddKeyword Keyword.Massive | n >= 3]
    _ -> pure mempty

instance RunMessage ShadowSpawned where
  runMessage msg t@(ShadowSpawned attrs) = runQueueT $ case msg of
    PlaceEnemyOutOfPlay VoidZone eid | toTarget eid `elem` attrs.attached -> pure t
    _ -> ShadowSpawned <$> liftRunMessage msg attrs
