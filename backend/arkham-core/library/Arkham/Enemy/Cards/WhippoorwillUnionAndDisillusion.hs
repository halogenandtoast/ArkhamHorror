module Arkham.Enemy.Cards.WhippoorwillUnionAndDisillusion
  ( WhippoorwillUnionAndDisillusion(..)
  , whippoorwillUnionAndDisillusion
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype WhippoorwillUnionAndDisillusion = WhippoorwillUnionAndDisillusion EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

whippoorwillUnionAndDisillusion :: EnemyCard WhippoorwillUnionAndDisillusion
whippoorwillUnionAndDisillusion = enemy WhippoorwillUnionAndDisillusion Cards.whippoorwillUnionAndDisillusion (2, Static 1, 4) (0, 1)

instance HasModifiersFor WhippoorwillUnionAndDisillusion where
  getModifiersFor (InvestigatorTarget iid) (WhippoorwillUnionAndDisillusion attrs) = do
    affected <- iid <=~> InvestigatorAt (locationWithEnemy $ toId attrs)
    pure $ toModifiers attrs [ AnySkillValue (-1) | affected ]
  getModifiersFor _ _ = pure []

instance RunMessage WhippoorwillUnionAndDisillusion where
  runMessage msg (WhippoorwillUnionAndDisillusion attrs) = WhippoorwillUnionAndDisillusion <$> runMessage msg attrs
