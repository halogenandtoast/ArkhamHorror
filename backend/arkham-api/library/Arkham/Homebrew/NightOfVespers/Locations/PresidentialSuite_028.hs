-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.PresidentialSuite_028 (presidentialSuite_028) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PresidentialSuite_028 = PresidentialSuite_028 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

presidentialSuite_028 :: LocationCard PresidentialSuite_028
presidentialSuite_028 = symbolLabel $ location PresidentialSuite_028 Cards.presidentialSuite_028 4 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities PresidentialSuite_028 where
  getAbilities (PresidentialSuite_028 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage PresidentialSuite_028 where
  runMessage msg l@(PresidentialSuite_028 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hotelRooftop
          , Cards.hedgeMaze
          ]
      pure l
    _ -> PresidentialSuite_028 <$> liftRunMessage msg attrs
