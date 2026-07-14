module Arkham.Homebrew.CircusExMortis.Locations.TheBigTopFirstRing (theBigTopFirstRing) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Creature, Monster))

-- "Moving between The Big Top locations does not provoke attacks of opportunity."
-- The engine never provokes attacks of opportunity on a move action, so this clause
-- is already satisfied; nothing to implement. See report note on the Big Top grouping.

newtype TheBigTopFirstRing = TheBigTopFirstRing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBigTopFirstRing :: LocationCard TheBigTopFirstRing
theBigTopFirstRing =
  location TheBigTopFirstRing Cards.theBigTopFirstRing 1 (PerPlayer 1)
    & setLabel "theBigTopFirstRing"

instance HasAbilities TheBigTopFirstRing where
  getAbilities (TheBigTopFirstRing a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage TheBigTopFirstRing where
  runMessage msg l@(TheBigTopFirstRing attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mEnemy <-
        selectOne
          $ NearestEnemyToLocation (toId attrs) (oneOf [EnemyWithTrait Creature, EnemyWithTrait Monster])
      for_ mEnemy \enemy -> do
        ready enemy
        push $ HunterMove enemy
      pure l
    _ -> TheBigTopFirstRing <$> liftRunMessage msg attrs
