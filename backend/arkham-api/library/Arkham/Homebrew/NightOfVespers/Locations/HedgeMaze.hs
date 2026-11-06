-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.HedgeMaze (hedgeMaze) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HedgeMaze = HedgeMaze LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hedgeMaze :: LocationCard HedgeMaze
hedgeMaze = symbolLabel $ location HedgeMaze Cards.hedgeMaze 4 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities HedgeMaze where
  getAbilities (HedgeMaze attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage HedgeMaze where
  runMessage msg l@(HedgeMaze attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.grandBallroom
          ]
      pure l
    _ -> HedgeMaze <$> liftRunMessage msg attrs
