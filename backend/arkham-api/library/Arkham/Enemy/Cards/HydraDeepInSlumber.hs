module Arkham.Enemy.Cards.HydraDeepInSlumber (hydraDeepInSlumber, HydraDeepInSlumber (..)) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Message (ReplaceStrategy (..))

newtype HydraDeepInSlumber = HydraDeepInSlumber EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hydraDeepInSlumber :: EnemyCard HydraDeepInSlumber
hydraDeepInSlumber = enemyWith HydraDeepInSlumber Cards.hydraDeepInSlumber (0, Static 1, 0) (0, 0)
  $ \a -> a {enemyFight = Nothing, enemyHealth = Nothing, enemyEvade = Nothing}

instance HasModifiersFor HydraDeepInSlumber where
  getModifiersFor target (HydraDeepInSlumber a) = maybeModified a do
    guard $ isTarget a target
    pure [Omnipotent]

instance RunMessage HydraDeepInSlumber where
  runMessage msg e@(HydraDeepInSlumber attrs) = runQueueT $ case msg of
    EnemyCheckEngagement eid | eid == attrs.id -> pure e
    Flip _ _ (isTarget attrs -> True) -> do
      awakened <- genCard Cards.hydraAwakenedAndEnraged
      push $ ReplaceEnemy attrs.id awakened Swap
      pure e
    _ -> HydraDeepInSlumber <$> liftRunMessage msg attrs
