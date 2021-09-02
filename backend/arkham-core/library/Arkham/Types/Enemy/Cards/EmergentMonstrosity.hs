module Arkham.Types.Enemy.Cards.EmergentMonstrosity
  ( EmergentMonstrosity(..)
  , emergentMonstrosity
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher

newtype EmergentMonstrosity = EmergentMonstrosity EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

emergentMonstrosity :: EnemyCard EmergentMonstrosity
emergentMonstrosity = enemyWith
  EmergentMonstrosity
  Cards.emergentMonstrosity
  (4, Static 5, 3)
  (2, 2)
  ((spawnAtL
   ?~ FirstLocation [LocationInDirection RightOf YourLocation, YourLocation]
   )
  . (exhaustedL .~ True)
  )

instance EnemyRunner env => RunMessage env EmergentMonstrosity where
  runMessage msg (EmergentMonstrosity attrs) =
    EmergentMonstrosity <$> runMessage msg attrs
