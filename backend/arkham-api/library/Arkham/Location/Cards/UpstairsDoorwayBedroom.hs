module Arkham.Location.Cards.UpstairsDoorwayBedroom (upstairsDoorwayBedroom, UpstairsDoorwayBedroom (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype UpstairsDoorwayBedroom = UpstairsDoorwayBedroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

upstairsDoorwayBedroom :: LocationCard UpstairsDoorwayBedroom
upstairsDoorwayBedroom = location UpstairsDoorwayBedroom Cards.upstairsDoorwayBedroom 3 (PerPlayer 1)

instance HasAbilities UpstairsDoorwayBedroom where
  getAbilities (UpstairsDoorwayBedroom x) =
    extendRevealed
      x
      [ restricted x 1 (Here <> exists (TreacheryInThreatAreaOf You <> TreacheryIsNonWeakness))
          $ ActionAbility [] (ActionCost 2)
      , restricted x 2 (Here <> not_ (Remembered FoundACrackedMirror))
          $ FastAbility
          $ GroupClueCost (PerPlayer 1) (be x)
      ]

instance RunMessage UpstairsDoorwayBedroom where
  runMessage msg l@(UpstairsDoorwayBedroom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      treacheries <- select $ treacheryInThreatAreaOf iid <> TreacheryIsNonWeakness
      for_ treacheries $ toDiscardBy iid attrs
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      remember FoundACrackedMirror
      pure l
    _ -> UpstairsDoorwayBedroom <$> liftRunMessage msg attrs
