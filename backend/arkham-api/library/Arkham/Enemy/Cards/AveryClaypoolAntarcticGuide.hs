module Arkham.Enemy.Cards.AveryClaypoolAntarcticGuide (averyClaypoolAntarcticGuide) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype AveryClaypoolAntarcticGuide = AveryClaypoolAntarcticGuide EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

averyClaypoolAntarcticGuide :: EnemyCard AveryClaypoolAntarcticGuide
averyClaypoolAntarcticGuide = enemy AveryClaypoolAntarcticGuide Cards.averyClaypoolAntarcticGuide (1, Static 3, 3) (2, 1)

instance HasAbilities AveryClaypoolAntarcticGuide where
  getAbilities (AveryClaypoolAntarcticGuide a) =
    extend
      a
      [ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_
      , mkAbility a 2 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage AveryClaypoolAntarcticGuide where
  runMessage msg e@(AveryClaypoolAntarcticGuide attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 1) iid sType (Fixed 5)
      doStep 2 msg
      pure e
    DoStep 2 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      n <- perPlayer 1
      when (attrs.token #resource >= n) $ addToVictory attrs
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      eliminatePartner attrs
      pure e
    _ -> AveryClaypoolAntarcticGuide <$> liftRunMessage msg attrs
