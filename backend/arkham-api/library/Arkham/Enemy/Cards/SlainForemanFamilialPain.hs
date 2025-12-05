module Arkham.Enemy.Cards.SlainForemanFamilialPain (slainForemanFamilialPain) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Story.Cards qualified as Stories

newtype SlainForemanFamilialPain = SlainForemanFamilialPain EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

slainForemanFamilialPain :: EnemyCard SlainForemanFamilialPain
slainForemanFamilialPain = enemy SlainForemanFamilialPain Cards.slainForemanFamilialPain (0, Static 1, 0) (0, 0)

instance RunMessage SlainForemanFamilialPain where
  runMessage msg e@(SlainForemanFamilialPain attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let familialPain = lookupCard Stories.familialPain (toCardId attrs)
      focusCards [familialPain] $ continue_ iid
      pure e
    _ -> SlainForemanFamilialPain <$> liftRunMessage msg attrs
