module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.ChildOfBlood (childOfBlood) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype ChildOfBlood = ChildOfBlood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

childOfBlood :: EnemyCard ChildOfBlood
childOfBlood =
  enemy ChildOfBlood Cards.childOfBlood
    & setPrey (InvestigatorWithMostSealedChaosToken #blood)
