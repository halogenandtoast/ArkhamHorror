module Arkham.Enemy.Cards.TerrorOfTheStarsBringerOfIceAndDeath (
  terrorOfTheStarsBringerOfIceAndDeath,
  TerrorOfTheStarsBringerOfIceAndDeath (..),
)
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Game.Helpers (onSameLocation)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMapM, modifySelf)
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
  getModifiersFor (TerrorOfTheStarsBringerOfIceAndDeath a) = do
    healthModifier <- perPlayer 3
    self <- modifySelf a [HealthModifier healthModifier]
    investigators <-
      if a.ready
        then modifySelectMapM a Anyone \iid -> do
          sameLocation <- onSameLocation iid a.placement
          pure $ CannotDiscoverCluesAt (locationWithEnemy a) : [CannotTakeAction #resign | sameLocation]
        else pure mempty
    pure $ self <> investigators

instance RunMessage TerrorOfTheStarsBringerOfIceAndDeath where
  runMessage msg (TerrorOfTheStarsBringerOfIceAndDeath attrs) = runQueueT $ case msg of
    _ -> TerrorOfTheStarsBringerOfIceAndDeath <$> liftRunMessage msg attrs
