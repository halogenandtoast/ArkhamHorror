module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.HowardWilkesUltimatePredator (howardWilkesUltimatePredator) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HowardWilkesUltimatePredator = HowardWilkesUltimatePredator EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

howardWilkesUltimatePredator :: EnemyCard HowardWilkesUltimatePredator
howardWilkesUltimatePredator = enemy HowardWilkesUltimatePredator Cards.howardWilkesUltimatePredator

instance RunMessage HowardWilkesUltimatePredator where
  runMessage msg (HowardWilkesUltimatePredator attrs) = HowardWilkesUltimatePredator <$> runMessage msg attrs
