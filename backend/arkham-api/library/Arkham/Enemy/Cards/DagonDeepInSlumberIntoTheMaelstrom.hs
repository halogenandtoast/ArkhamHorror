module Arkham.Enemy.Cards.DagonDeepInSlumberIntoTheMaelstrom
  ( dagonDeepInSlumberIntoTheMaelstrom
  , DagonDeepInSlumberIntoTheMaelstrom(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DagonDeepInSlumberIntoTheMaelstrom = DagonDeepInSlumberIntoTheMaelstrom EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dagonDeepInSlumberIntoTheMaelstrom :: EnemyCard DagonDeepInSlumberIntoTheMaelstrom
dagonDeepInSlumberIntoTheMaelstrom = enemy DagonDeepInSlumberIntoTheMaelstrom Cards.dagonDeepInSlumberIntoTheMaelstrom (0, Static 1, 0) (0, 0)

instance RunMessage DagonDeepInSlumberIntoTheMaelstrom where
  runMessage msg (DagonDeepInSlumberIntoTheMaelstrom attrs) = runQueueT $ case msg of
    _ -> DagonDeepInSlumberIntoTheMaelstrom <$> liftRunMessage msg attrs
