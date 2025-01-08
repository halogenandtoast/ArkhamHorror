module Arkham.Enemy.Cards.OBannionsThug (oBannionsThug, OBannionsThug (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher

newtype OBannionsThug = OBannionsThug EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

oBannionsThug :: EnemyCard OBannionsThug
oBannionsThug = enemy OBannionsThug Cards.oBannionsThug (4, Static 2, 2) (2, 0)

instance HasModifiersFor OBannionsThug where
  getModifiersFor (OBannionsThug a) = do
    modifySelect a (investigatorEngagedWith a) [CannotGainResources]

instance RunMessage OBannionsThug where
  runMessage msg (OBannionsThug attrs) = OBannionsThug <$> runMessage msg attrs
