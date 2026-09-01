module Arkham.Homebrew.CircusExMortis.Locations.TheBigTopFirstRing (theBigTopFirstRing) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (bigTopRings)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Creature, Monster))

newtype TheBigTopFirstRing = TheBigTopFirstRing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBigTopFirstRing :: LocationCard TheBigTopFirstRing
theBigTopFirstRing =
  location TheBigTopFirstRing Cards.theBigTopFirstRing 1 (PerPlayer 1)
    & setLabel "theBigTopFirstRing"

instance HasModifiersFor TheBigTopFirstRing where
  getModifiersFor (TheBigTopFirstRing a) =
    modifySelect a (investigatorAt a.id) [MovingToDoesNotProvokeAttacksOfOpportunity bigTopRings]

instance HasAbilities TheBigTopFirstRing where
  getAbilities (TheBigTopFirstRing a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage TheBigTopFirstRing where
  runMessage msg l@(TheBigTopFirstRing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ NearestEnemyToLocation attrs.id $ mapOneOf withTrait [Creature, Monster]
      chooseOrRunOneM iid $ targets enemies \enemy -> do
        ready enemy
        resolveHunterKeyword enemy
      pure l
    _ -> TheBigTopFirstRing <$> liftRunMessage msg attrs
