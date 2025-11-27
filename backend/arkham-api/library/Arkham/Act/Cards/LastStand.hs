module Arkham.Act.Cards.LastStand (lastStand) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype LastStand = LastStand ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lastStand :: ActCard LastStand
lastStand = act (4, A) LastStand Cards.lastStand Nothing

instance HasAbilities LastStand where
  getAbilities = actAbilities1 \a ->
    mkAbility a 1 $ Objective $ forced $ ifEnemyDefeated Enemies.voidChimeraTrueForm

instance RunMessage LastStand where
  runMessage msg a@(LastStand attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      assetVersion <- selectAny $ assetIs Assets.thorneConsummateProfessional
      enemyVersion <- selectAny $ enemyIs Enemies.thorneTheOneWithTheRedCravat

      push
        $ if
          | assetVersion -> R1
          | enemyVersion -> R2
          | otherwise -> R3
      pure a
    _ -> LastStand <$> liftRunMessage msg attrs
