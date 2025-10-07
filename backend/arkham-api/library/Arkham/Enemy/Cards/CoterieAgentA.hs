module Arkham.Enemy.Cards.CoterieAgentA (coterieAgentA) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype CoterieAgentA = CoterieAgentA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coterieAgentA :: EnemyCard CoterieAgentA
coterieAgentA = enemy CoterieAgentA Cards.coterieAgentA (1, Static 1, 1) (1, 0)

instance HasAbilities CoterieAgentA where
  getAbilities (CoterieAgentA a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyPlaced #after InTheShadows (be a)
      , mkAbility a 2 $ freeReaction (whenExposed a)
      ]

instance RunMessage CoterieAgentA where
  runMessage msg e@(CoterieAgentA attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom attrs attrs 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure e
    _ -> CoterieAgentA <$> liftRunMessage msg attrs
