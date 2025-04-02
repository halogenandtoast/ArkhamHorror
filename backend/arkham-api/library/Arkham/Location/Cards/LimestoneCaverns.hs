module Arkham.Location.Cards.LimestoneCaverns (limestoneCaverns) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheHeartOfMadness.Helpers

newtype LimestoneCaverns = LimestoneCaverns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

limestoneCaverns :: LocationCard LimestoneCaverns
limestoneCaverns = location LimestoneCaverns Cards.limestoneCaverns 2 (Static 0)

instance HasAbilities LimestoneCaverns where
  getAbilities (LimestoneCaverns a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 Here
          $ actionAbilityWithCost
          $ GroupClueCost (PerPlayer 1) (be a)
          <> GroupResourceCost (PerPlayer 3) (be a)
      ]

instance RunMessage LimestoneCaverns where
  runMessage msg l@(LimestoneCaverns attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeSeal attrs (Seal SealA False)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      for_ (nonEmpty $ toList attrs.seals) \(k :| _) -> do
        placeSeal iid k
      pure l
    _ -> LimestoneCaverns <$> liftRunMessage msg attrs
