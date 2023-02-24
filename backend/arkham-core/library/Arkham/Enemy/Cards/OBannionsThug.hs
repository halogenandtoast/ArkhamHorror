module Arkham.Enemy.Cards.OBannionsThug
  ( oBannionsThug
  , OBannionsThug(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype OBannionsThug = OBannionsThug EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

oBannionsThug :: EnemyCard OBannionsThug
oBannionsThug = enemy OBannionsThug Cards.oBannionsThug (4, Static 2, 2) (2, 0)

instance HasModifiersFor OBannionsThug where
  getModifiersFor (InvestigatorTarget iid) (OBannionsThug a@EnemyAttrs {..}) = do
    affected <- iid <=~> investigatorEngagedWith enemyId
    pure $ toModifiers a [CannotGainResources | affected]
  getModifiersFor _ _ = pure []

instance RunMessage OBannionsThug where
  runMessage msg (OBannionsThug attrs) = OBannionsThug <$> runMessage msg attrs
