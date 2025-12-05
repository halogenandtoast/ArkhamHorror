module Arkham.Enemy.Cards.SlainForemanSympathyPain (slainForemanSympathyPain) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Story.Cards qualified as Stories

newtype SlainForemanSympathyPain = SlainForemanSympathyPain EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

slainForemanSympathyPain :: EnemyCard SlainForemanSympathyPain
slainForemanSympathyPain = enemy SlainForemanSympathyPain Cards.slainForemanSympathyPain (0, Static 1, 0) (0, 0)

instance RunMessage SlainForemanSympathyPain where
  runMessage msg e@(SlainForemanSympathyPain attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let sympathyPain = lookupCard Stories.sympathyPain (toCardId attrs)
      focusCards [sympathyPain] $ continue_ iid
      pure e
    _ -> SlainForemanSympathyPain <$> liftRunMessage msg attrs
