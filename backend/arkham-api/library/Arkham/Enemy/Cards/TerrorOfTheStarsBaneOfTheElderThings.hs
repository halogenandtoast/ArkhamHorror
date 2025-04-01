module Arkham.Enemy.Cards.TerrorOfTheStarsBaneOfTheElderThings (terrorOfTheStarsBaneOfTheElderThings) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMaybe, modifySelf)
import Arkham.Matcher

newtype TerrorOfTheStarsBaneOfTheElderThings = TerrorOfTheStarsBaneOfTheElderThings EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

terrorOfTheStarsBaneOfTheElderThings :: EnemyCard TerrorOfTheStarsBaneOfTheElderThings
terrorOfTheStarsBaneOfTheElderThings =
  enemy
    TerrorOfTheStarsBaneOfTheElderThings
    Cards.terrorOfTheStarsBaneOfTheElderThings
    (4, Static 3, 3)
    (2, 2)

instance HasModifiersFor TerrorOfTheStarsBaneOfTheElderThings where
  getModifiersFor (TerrorOfTheStarsBaneOfTheElderThings a) = do
    healthModifier <- perPlayer 3
    modifySelf a [HealthModifier healthModifier]
    when a.ready do
      modifySelectMaybe a Anyone \iid -> do
        liftGuardM $ onSameLocation iid a.placement
        pure [CannotSpendKeys, CannotTakeKeys]

instance RunMessage TerrorOfTheStarsBaneOfTheElderThings where
  runMessage msg (TerrorOfTheStarsBaneOfTheElderThings attrs) =
    TerrorOfTheStarsBaneOfTheElderThings <$> runMessage msg attrs
