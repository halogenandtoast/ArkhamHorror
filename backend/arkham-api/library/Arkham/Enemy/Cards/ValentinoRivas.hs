module Arkham.Enemy.Cards.ValentinoRivas (valentinoRivas) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype ValentinoRivas = ValentinoRivas EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valentinoRivas :: EnemyCard ValentinoRivas
valentinoRivas = enemy ValentinoRivas Cards.valentinoRivas (3, Static 5, 4) (1, 1)

instance HasAbilities ValentinoRivas where
  getAbilities (ValentinoRivas a) =
    extend1 a
      $ restricted a 1 (youExist InvestigatorWithAnyResources)
      $ forced
      $ EnemyEngaged #after You (be a)

instance RunMessage ValentinoRivas where
  runMessage msg e@(ValentinoRivas attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseResources iid (attrs.ability 1) 2
      pure e
    _ -> ValentinoRivas <$> liftRunMessage msg attrs
