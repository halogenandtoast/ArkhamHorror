module Arkham.Homebrew.CircusExMortis.Locations.TheBigTopSecondRing (theBigTopSecondRing) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Performer))

-- "Moving between The Big Top locations does not provoke attacks of opportunity."
-- The engine never provokes attacks of opportunity on a move action, so this clause
-- is already satisfied; nothing to implement. See report note on the Big Top grouping.

newtype TheBigTopSecondRing = TheBigTopSecondRing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBigTopSecondRing :: LocationCard TheBigTopSecondRing
theBigTopSecondRing =
  location TheBigTopSecondRing Cards.theBigTopSecondRing 2 (PerPlayer 1)
    & setLabel "theBigTopSecondRing"

instance HasAbilities TheBigTopSecondRing where
  getAbilities (TheBigTopSecondRing a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage TheBigTopSecondRing where
  runMessage msg l@(TheBigTopSecondRing attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mEnemy <- selectOne $ NearestEnemyToLocation (toId attrs) (EnemyWithTrait Performer)
      for_ mEnemy \enemy -> do
        ready enemy
        push $ HunterMove enemy
      pure l
    _ -> TheBigTopSecondRing <$> liftRunMessage msg attrs
