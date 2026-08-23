module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.HowardWilkesFirstChildOfZburamoarte (howardWilkesFirstChildOfZburamoarte) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HowardWilkesFirstChildOfZburamoarte = HowardWilkesFirstChildOfZburamoarte EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

howardWilkesFirstChildOfZburamoarte :: EnemyCard HowardWilkesFirstChildOfZburamoarte
howardWilkesFirstChildOfZburamoarte = enemy HowardWilkesFirstChildOfZburamoarte Cards.howardWilkesFirstChildOfZburamoarte

instance RunMessage HowardWilkesFirstChildOfZburamoarte where
  runMessage msg (HowardWilkesFirstChildOfZburamoarte attrs) = HowardWilkesFirstChildOfZburamoarte <$> runMessage msg attrs
