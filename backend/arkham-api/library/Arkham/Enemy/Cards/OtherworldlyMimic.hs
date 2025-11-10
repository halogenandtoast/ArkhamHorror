module Arkham.Enemy.Cards.OtherworldlyMimic (otherworldlyMimic) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (PlayCard)
import Arkham.Matcher

newtype OtherworldlyMimic = OtherworldlyMimic EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyMimic :: EnemyCard OtherworldlyMimic
otherworldlyMimic = enemy OtherworldlyMimic Cards.otherworldlyMimic (3, Static 2, 4) (0, 2)

instance HasAbilities OtherworldlyMimic where
  getAbilities (OtherworldlyMimic a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ oneOf
        [ CommittedCard #when (You <> InvestigatorAt (locationWithEnemy a)) CardWithHollowedCopy
        , PlayCard #when (You <> InvestigatorAt (locationWithEnemy a)) CardWithHollowedCopy
        , ActivateAbility
            #when
            (You <> InvestigatorAt (locationWithEnemy a))
            (AbilityOnExtendedCard CardWithHollowedCopy)
        ]

instance RunMessage OtherworldlyMimic where
  runMessage msg e@(OtherworldlyMimic attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> OtherworldlyMimic <$> liftRunMessage msg attrs
