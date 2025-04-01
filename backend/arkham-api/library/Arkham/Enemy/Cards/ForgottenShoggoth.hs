module Arkham.Enemy.Cards.ForgottenShoggoth (forgottenShoggoth) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype ForgottenShoggoth = ForgottenShoggoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forgottenShoggoth :: EnemyCard ForgottenShoggoth
forgottenShoggoth = enemy ForgottenShoggoth Cards.forgottenShoggoth (3, Static 6, 2) (1, 2)

instance HasAbilities ForgottenShoggoth where
  getAbilities (ForgottenShoggoth a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #when You (be a)

instance RunMessage ForgottenShoggoth where
  runMessage msg e@(ForgottenShoggoth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      tekelili <- getTekelili 1

      if null tekelili
        then initiateEnemyAttack attrs (attrs.ability 1) iid
        else drawTekelili iid (attrs.ability 1) 1
      pure e
    _ -> ForgottenShoggoth <$> liftRunMessage msg attrs
