module Arkham.Location.Cards.RiverDocksPast (riverDocksPast) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Scientist))

newtype RiverDocksPast = RiverDocksPast LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverDocksPast :: LocationCard RiverDocksPast
riverDocksPast = location RiverDocksPast Cards.riverDocksPast 1 (PerPlayer 1)

instance HasAbilities RiverDocksPast where
  getAbilities (RiverDocksPast a) =
    extendRevealed1 a
      $ groupLimit PerTurn
      $ restricted a 1 Here
      $ actionAbilityWithCost
      $ OrCost [ResourceCost 2, ExhaustAssetCost (AssetWithTrait Scientist <> AssetAt (be a))]

instance RunMessage RiverDocksPast where
  runMessage msg l@(RiverDocksPast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      docks <-
        select $ mapOneOf locationIs [Cards.riverDocksPresent, Cards.riverDocksFuture]
      chooseOrRunOneM iid do
        for_ docks \dock ->
          targeting dock $ placeTokens (attrs.ability 1) dock Token.Shipment 1
      pure l
    _ -> RiverDocksPast <$> liftRunMessage msg attrs
