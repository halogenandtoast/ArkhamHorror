module Arkham.Enemy.Cards.JosefMeiger
  ( josefMeiger
  , JosefMeiger(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype JosefMeiger = JosefMeiger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

josefMeiger :: EnemyCard JosefMeiger
josefMeiger = enemy JosefMeiger Cards.josefMeiger (3, Static 3, 3) (1, 1)

instance RunMessage JosefMeiger where
  runMessage msg (JosefMeiger attrs) =
    JosefMeiger <$> runMessage msg attrs
