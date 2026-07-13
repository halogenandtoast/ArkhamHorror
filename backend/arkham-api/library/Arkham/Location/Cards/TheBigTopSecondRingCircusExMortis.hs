module Arkham.Location.Cards.TheBigTopSecondRingCircusExMortis (theBigTopSecondRingCircusExMortis) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Performer))

-- "Moving between The Big Top locations does not provoke attacks of opportunity."
-- The engine never provokes attacks of opportunity on a move action, so this clause
-- is already satisfied; nothing to implement. See report note on the Big Top grouping.

newtype TheBigTopSecondRingCircusExMortis = TheBigTopSecondRingCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBigTopSecondRingCircusExMortis :: LocationCard TheBigTopSecondRingCircusExMortis
theBigTopSecondRingCircusExMortis =
  location TheBigTopSecondRingCircusExMortis Cards.theBigTopSecondRingCircusExMortis 2 (PerPlayer 1)

instance HasAbilities TheBigTopSecondRingCircusExMortis where
  getAbilities (TheBigTopSecondRingCircusExMortis a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage TheBigTopSecondRingCircusExMortis where
  runMessage msg l@(TheBigTopSecondRingCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mEnemy <- selectOne $ NearestEnemyToLocation (toId attrs) (EnemyWithTrait Performer)
      for_ mEnemy \enemy -> do
        ready enemy
        push $ HunterMove enemy
      pure l
    _ -> TheBigTopSecondRingCircusExMortis <$> liftRunMessage msg attrs
