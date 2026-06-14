module Arkham.Location.Cards.BarrierCoreActive (barrierCoreActive) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Trait (Trait (Seafloor))

newtype BarrierCoreActive = BarrierCoreActive LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barrierCoreActive :: LocationCard BarrierCoreActive
barrierCoreActive = location BarrierCoreActive Cards.barrierCoreActive 4 (Static 0)

instance HasAbilities BarrierCoreActive where
  getAbilities (BarrierCoreActive a) =
    extendRevealed
      a
      [ restricted a 1 NoRestriction $ forced $ FloodLevelChanged #after Anywhere
      , restricted a 2 Here
          $ actionAbilityWithCost
          $ GroupClueCost (PerPlayer 2) (be a)
      ]

instance RunMessage BarrierCoreActive where
  runMessage msg l@(BarrierCoreActive attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      unfloodedSeafloor <- selectCount $ withTrait Seafloor <> not_ FloodedLocation
      when (unfloodedSeafloor <= 5) do
        let inactive = lookupCard Cards.barrierCoreInactive attrs.cardId
        push $ ReplaceLocation attrs.id inactive Swap
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.barrierNode
      pure l
    _ -> BarrierCoreActive <$> liftRunMessage msg attrs
