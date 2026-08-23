module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.HowardWilkesTooFarGone (howardWilkesTooFarGone) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HowardWilkesTooFarGone = HowardWilkesTooFarGone EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

howardWilkesTooFarGone :: EnemyCard HowardWilkesTooFarGone
howardWilkesTooFarGone = enemy HowardWilkesTooFarGone Cards.howardWilkesTooFarGone

instance RunMessage HowardWilkesTooFarGone where
  runMessage msg (HowardWilkesTooFarGone attrs) = HowardWilkesTooFarGone <$> runMessage msg attrs
