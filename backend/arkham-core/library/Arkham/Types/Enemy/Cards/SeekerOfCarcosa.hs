module Arkham.Types.Enemy.Cards.SeekerOfCarcosa
  ( seekerOfCarcosa
  , SeekerOfCarcosa(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher

newtype SeekerOfCarcosa = SeekerOfCarcosa EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

seekerOfCarcosa :: EnemyCard SeekerOfCarcosa
seekerOfCarcosa = enemyWith
  SeekerOfCarcosa
  Cards.seekerOfCarcosa
  (2, Static 3, 2)
  (0, 1)
  (spawnAtL ?~ EmptyLocation <> LocationWithTitle "Historical Society")

instance EnemyRunner env => RunMessage env SeekerOfCarcosa where
  runMessage msg (SeekerOfCarcosa attrs) =
    SeekerOfCarcosa <$> runMessage msg attrs
