module Arkham.Act.Cards.HarvesterOfWoe (harvesterOfWoe) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Window (dealtDamage, dealtHorror)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ShadesOfSuffering.Helpers
import Arkham.Window qualified as W

newtype HarvesterOfWoe = HarvesterOfWoe ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harvesterOfWoe :: ActCard HarvesterOfWoe
harvesterOfWoe = act (3, A) HarvesterOfWoe Cards.harvesterOfWoe Nothing

instance HasAbilities HarvesterOfWoe where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ triggered
          ( DealtDamageOrHorror
              #when
              (SourceIsCancelable AnySource <> SourceIsEnemy (enemyIs Enemies.tzuSanNiangOutForBlood))
              You
          )
          (GroupClueCostX YourLocation)
    , mkAbility a 2
        $ Objective
        $ forced
        $ IfEnemyDefeated #after Anyone ByAny (enemyIs Enemies.tzuSanNiangOutForBlood)
    ]

instance RunMessage HarvesterOfWoe where
  runMessage msg a@(HarvesterOfWoe attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      leadChooseOneM $ scenarioI18n do
        labeled' "harvesterOfWoe.useTheParasol" $ push R1
        labeled' "harvesterOfWoe.letHerGo" $ push R2
      pure a
    UseCardAbility _iid (isSource attrs -> True) 1 ws (totalCluePayment -> n) -> do
      let (damage, horror) = (dealtDamage ws, dealtHorror ws)
      when (n > 0) $ doStep n (DoStep damage (DoStep horror msg))
      pure a
    DoStep n (DoStep damage (DoStep horror msg'@(UseThisAbility iid (isSource attrs -> True) 1))) -> do
      when (n > 0 && damage + horror > 0) do
        if damage > 0 && horror > 0
          then chooseOneM iid do
            labeled "Cancel 1 damage" do
              cancelInvestigatorDamage iid 1
              doStep (n - 1) (DoStep (damage - 1) (DoStep horror msg'))
            labeled "Cancel 1 horror" do
              cancelInvestigatorHorror iid 1
              doStep (n - 1) (DoStep damage (DoStep (horror - 1) msg'))
          else do
            cancelInvestigatorDamage iid (min n damage)
            cancelInvestigatorHorror iid (min n horror)
        checkAfter $ W.CancelledOrIgnoredCardOrGameEffect (attrs.ability 1) Nothing
      pure a
    _ -> HarvesterOfWoe <$> liftRunMessage msg attrs
