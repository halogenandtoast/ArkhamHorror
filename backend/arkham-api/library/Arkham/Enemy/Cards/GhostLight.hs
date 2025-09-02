module Arkham.Enemy.Cards.GhostLight (ghostLight) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait

newtype GhostLight = GhostLight EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghostLight :: EnemyCard GhostLight
ghostLight = enemy GhostLight Cards.ghostLight (2, Static 2, 2) (0, 1)

instance HasModifiersFor GhostLight where
  getModifiersFor (GhostLight a) = do
    modifySelf
      a
      [ CannotBeDamagedByPlayerSourcesExcept $ SourceMatchesAny $ map SourceWithTrait [Spell, Relic, Charm]
      ]

instance HasAbilities GhostLight where
  getAbilities (GhostLight a) = extend1 a $ mkAbility a 1 $ freeReaction $ EnemyEvaded #after Anyone (be a)

instance RunMessage GhostLight where
  runMessage msg e@(GhostLight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 attrs
      pure e
    _ -> GhostLight <$> liftRunMessage msg attrs
