-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.PresidentialSuite_027 (presidentialSuite_027) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PresidentialSuite_027 = PresidentialSuite_027 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

presidentialSuite_027 :: LocationCard PresidentialSuite_027
presidentialSuite_027 = symbolLabel $ location PresidentialSuite_027 Cards.presidentialSuite_027 4 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities PresidentialSuite_027 where
  getAbilities (PresidentialSuite_027 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage PresidentialSuite_027 where
  runMessage msg l@(PresidentialSuite_027 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hotelRooftop
          , Cards.hedgeMaze
          ]
      pure l
    _ -> PresidentialSuite_027 <$> liftRunMessage msg attrs
