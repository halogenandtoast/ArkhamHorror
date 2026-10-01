module Arkham.Homebrew.CircusExMortis.Locations.HighThicket (highThicket) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (scatterTowardSilentClearing, shubNiggurathLeaves)
import Arkham.Location.Import.Lifted

newtype HighThicket = HighThicket LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highThicket :: LocationCard HighThicket
highThicket = location HighThicket Cards.highThicket 2 (PerPlayer 1)

-- Both faces print the same Forced ability, so it is not gated on being revealed.
instance HasAbilities HighThicket where
  getAbilities (HighThicket a) = extend1 a (shubNiggurathLeaves 1 a)

instance RunMessage HighThicket where
  runMessage msg l@(HighThicket attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      scatterTowardSilentClearing attrs
      -- "Remove this location from the game", never the victory display.
      removeLocationWithoutVictory attrs
      pure l
    _ -> HighThicket <$> liftRunMessage msg attrs
