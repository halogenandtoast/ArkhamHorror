module Arkham.Enemy.Cards.EliyahAshevakDogHandler
  ( eliyahAshevakDogHandler
  , EliyahAshevakDogHandler(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype EliyahAshevakDogHandler = EliyahAshevakDogHandler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

eliyahAshevakDogHandler :: EnemyCard EliyahAshevakDogHandler
eliyahAshevakDogHandler = enemy EliyahAshevakDogHandler Cards.eliyahAshevakDogHandler (0, Static 1, 0) (0, 0)

instance RunMessage EliyahAshevakDogHandler where
  runMessage msg (EliyahAshevakDogHandler attrs) = runQueueT $ case msg of
    _ -> EliyahAshevakDogHandler <$> liftRunMessage msg attrs
