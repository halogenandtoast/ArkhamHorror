module Arkham.Enemy.Cards.TerrorOfTheStarsBringerOfIceAndDeath (
  terrorOfTheStarsBringerOfIceAndDeath,
  TerrorOfTheStarsBringerOfIceAndDeath (..),
)
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Game.Helpers (onSameLocation)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified, modified)
import Arkham.Matcher

newtype TerrorOfTheStarsBringerOfIceAndDeath = TerrorOfTheStarsBringerOfIceAndDeath EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

terrorOfTheStarsBringerOfIceAndDeath :: EnemyCard TerrorOfTheStarsBringerOfIceAndDeath
terrorOfTheStarsBringerOfIceAndDeath =
  enemy
    TerrorOfTheStarsBringerOfIceAndDeath
    Cards.terrorOfTheStarsBringerOfIceAndDeath
    (4, Static 3, 3)
    (2, 2)

instance HasModifiersFor TerrorOfTheStarsBringerOfIceAndDeath where
  getModifiersFor target (TerrorOfTheStarsBringerOfIceAndDeath a) | isTarget a target = do
    healthModifier <- perPlayer 3
    modified a [HealthModifier healthModifier]
  getModifiersFor (InvestigatorTarget iid) (TerrorOfTheStarsBringerOfIceAndDeath a) = maybeModified a do
    liftGuardM $ a.id <=~> ReadyEnemy
    sameLocation <- lift $ onSameLocation iid a.placement
    pure $ CannotDiscoverCluesAt (locationWithEnemy a) : [CannotTakeAction #resign | sameLocation]
  getModifiersFor _ _ = pure []

instance RunMessage TerrorOfTheStarsBringerOfIceAndDeath where
  runMessage msg (TerrorOfTheStarsBringerOfIceAndDeath attrs) = runQueueT $ case msg of
    _ -> TerrorOfTheStarsBringerOfIceAndDeath <$> liftRunMessage msg attrs
