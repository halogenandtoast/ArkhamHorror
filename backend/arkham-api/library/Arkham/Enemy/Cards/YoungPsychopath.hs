module Arkham.Enemy.Cards.YoungPsychopath (youngPsychopath, YoungPsychopath (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier qualified as Modifier

newtype YoungPsychopath = YoungPsychopath EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youngPsychopath :: EnemyCard YoungPsychopath
youngPsychopath =
  enemy YoungPsychopath Cards.youngPsychopath (2, Static 2, 3) (1, 1)

instance HasAbilities YoungPsychopath where
  getAbilities (YoungPsychopath a) = extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)

instance RunMessage YoungPsychopath where
  runMessage msg e@(YoungPsychopath attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        labeled "Take 1 Horror" $ assignHorror iid (attrs.ability 1) 1
        labeled "Young Psycopath gets +3 fight until the end of the investigation phase" do
          endOfPhaseModifier #investigation (attrs.ability 1) attrs (Modifier.EnemyFight 3)
      pure e
    _ -> YoungPsychopath <$> liftRunMessage msg attrs
