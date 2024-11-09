module Arkham.Enemy.Cards.DagonAwakenedAndEnragedIntoTheMaelstrom
  ( dagonAwakenedAndEnragedIntoTheMaelstrom
  , DagonAwakenedAndEnragedIntoTheMaelstrom(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DagonAwakenedAndEnragedIntoTheMaelstrom = DagonAwakenedAndEnragedIntoTheMaelstrom EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dagonAwakenedAndEnragedIntoTheMaelstrom :: EnemyCard DagonAwakenedAndEnragedIntoTheMaelstrom
dagonAwakenedAndEnragedIntoTheMaelstrom = enemy DagonAwakenedAndEnragedIntoTheMaelstrom Cards.dagonAwakenedAndEnragedIntoTheMaelstrom (7, Static 1, 4) (1, 2)

instance RunMessage DagonAwakenedAndEnragedIntoTheMaelstrom where
  runMessage msg (DagonAwakenedAndEnragedIntoTheMaelstrom attrs) = runQueueT $ case msg of
    _ -> DagonAwakenedAndEnragedIntoTheMaelstrom <$> liftRunMessage msg attrs
