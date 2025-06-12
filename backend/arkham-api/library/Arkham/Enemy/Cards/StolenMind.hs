module Arkham.Enemy.Cards.StolenMind (stolenMind) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Matcher

newtype StolenMind = StolenMind EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stolenMind :: EnemyCard StolenMind
stolenMind =
  enemy StolenMind Cards.stolenMind (1, Static 4, 3) (2, 0)
    & setSpawnAt EmptyLocation

instance HasModifiersFor StolenMind where
  getModifiersFor (StolenMind a) = do
    modifySelfWhen a (a.doom > 0) [EnemyFight a.doom]

instance HasAbilities StolenMind where
  getAbilities (StolenMind a) =
    extend1 a $ restricted a 1 CanPlaceDoomOnThis $ forced $ PhaseEnds #when #mythos

instance RunMessage StolenMind where
  runMessage msg e@(StolenMind attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> StolenMind <$> liftRunMessage msg attrs
