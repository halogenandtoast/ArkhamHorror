module Arkham.Location.Cards.UpstairsDoorwayBedroom (upstairsDoorwayBedroom, UpstairsDoorwayBedroom (..)) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
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
      [ restrictedAbility x 1 (Here <> exists (TreacheryInThreatAreaOf You <> TreacheryIsNonWeakness))
          $ ActionAbility [] (ActionCost 2)
      , restrictedAbility x 2 (Here <> not_ (Remembered FoundACrackedMirror))
          $ FastAbility
          $ GroupClueCost (PerPlayer 1) (LocationWithId $ toId x)
      ]

instance RunMessage UpstairsDoorwayBedroom where
  runMessage msg l@(UpstairsDoorwayBedroom attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      treacheries <- select $ treacheryInThreatAreaOf iid <> TreacheryIsNonWeakness
      for_ treacheries $ push . toDiscardBy iid attrs
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ Remember FoundACrackedMirror
      pure l
    _ -> UpstairsDoorwayBedroom <$> runMessage msg attrs
