module Arkham.Act.Cards.TheFinalErr (theFinalErr) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype TheFinalErr = TheFinalErr ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalErr :: ActCard TheFinalErr
theFinalErr = act (4, A) TheFinalErr Cards.theFinalErr Nothing

instance HasAbilities TheFinalErr where
  getAbilities = actAbilities1 \a ->
    mkAbility a 1
      $ Objective
      $ forced
      $ IfEnemyDefeated #after Anyone ByAny (enemyIs Enemies.mimeticNemesisInfiltratorOfRealities)

instance RunMessage TheFinalErr where
  runMessage msg a@(TheFinalErr attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> TheFinalErr <$> liftRunMessage msg attrs
