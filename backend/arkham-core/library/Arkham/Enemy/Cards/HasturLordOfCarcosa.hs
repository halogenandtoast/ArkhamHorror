module Arkham.Enemy.Cards.HasturLordOfCarcosa
  ( hasturLordOfCarcosa
  , HasturLordOfCarcosa(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype HasturLordOfCarcosa = HasturLordOfCarcosa EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hasturLordOfCarcosa :: EnemyCard HasturLordOfCarcosa
hasturLordOfCarcosa = enemy HasturLordOfCarcosa Cards.hasturLordOfCarcosa (3, PerPlayer 9, 3) (0, 2)

instance RunMessage HasturLordOfCarcosa where
  runMessage msg (HasturLordOfCarcosa attrs) =
    HasturLordOfCarcosa <$> runMessage msg attrs
