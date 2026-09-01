module Arkham.Homebrew.CircusExMortis.Locations.TheBigTopSecondRing (theBigTopSecondRing) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (bigTopRings)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Performer))

newtype TheBigTopSecondRing = TheBigTopSecondRing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBigTopSecondRing :: LocationCard TheBigTopSecondRing
theBigTopSecondRing =
  location TheBigTopSecondRing Cards.theBigTopSecondRing 2 (PerPlayer 1)
    & setLabel "theBigTopSecondRing"

instance HasModifiersFor TheBigTopSecondRing where
  getModifiersFor (TheBigTopSecondRing a) =
    modifySelect a (investigatorAt a.id) [MovingToDoesNotProvokeAttacksOfOpportunity bigTopRings]

instance HasAbilities TheBigTopSecondRing where
  getAbilities (TheBigTopSecondRing a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage TheBigTopSecondRing where
  runMessage msg l@(TheBigTopSecondRing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ NearestEnemyToLocation (toId attrs) (withTrait Performer)
      chooseOrRunOneM iid $ targets enemies \enemy -> do
        ready enemy
        resolveHunterKeyword enemy
      pure l
    _ -> TheBigTopSecondRing <$> liftRunMessage msg attrs
