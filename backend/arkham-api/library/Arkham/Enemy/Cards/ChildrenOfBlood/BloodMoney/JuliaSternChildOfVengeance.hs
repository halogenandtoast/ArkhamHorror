module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.JuliaSternChildOfVengeance (juliaSternChildOfVengeance) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype JuliaSternChildOfVengeance = JuliaSternChildOfVengeance EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

juliaSternChildOfVengeance :: EnemyCard JuliaSternChildOfVengeance
juliaSternChildOfVengeance = enemy JuliaSternChildOfVengeance Cards.juliaSternChildOfVengeance

instance RunMessage JuliaSternChildOfVengeance where
  runMessage msg (JuliaSternChildOfVengeance attrs) = JuliaSternChildOfVengeance <$> runMessage msg attrs
