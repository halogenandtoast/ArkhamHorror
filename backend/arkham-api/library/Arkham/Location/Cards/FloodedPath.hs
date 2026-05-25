module Arkham.Location.Cards.FloodedPath (floodedPath) where

import Arkham.Ability
import Arkham.Card
import Arkham.ForMovement
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Token (Token (..))

newtype FloodedPath = FloodedPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floodedPath :: LocationCard FloodedPath
floodedPath = locationWith FloodedPath Cards.floodedPath 1 (Static 0) connectsToAdjacent

instance HasAbilities FloodedPath where
  getAbilities (FloodedPath a) =
    extendRevealed1 a $ restricted a 1 (Here <> DuringTurn You) $ FastAbility Free

instance RunMessage FloodedPath where
  runMessage msg l@(FloodedPath attrs) = runQueueT $ case msg of
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.openWater10596b
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Damage 1
      locations <- select $ AccessibleFrom ForMovement (be attrs)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure l
    _ -> FloodedPath <$> liftRunMessage msg attrs
