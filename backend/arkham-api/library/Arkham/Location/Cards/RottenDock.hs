module Arkham.Location.Cards.RottenDock (rottenDock) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..))

newtype RottenDock = RottenDock LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rottenDock :: LocationCard RottenDock
rottenDock = locationWith RottenDock Cards.rottenDock 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RottenDock where
  getAbilities (RottenDock a) =
    extendRevealed1 a
      $ forcedAbility a 1
      $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage RottenDock where
  runMessage msg l@(RottenDock attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.openWater10598b
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    FailedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs Damage 1
      pure l
    _ -> RottenDock <$> liftRunMessage msg attrs
