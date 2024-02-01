module Arkham.Treachery.Cards.AcrossSpaceAndTime (
  acrossSpaceAndTime,
  AcrossSpaceAndTime (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Treachery.Cards qualified as Cards (acrossSpaceAndTime)
import Arkham.Treachery.Runner

newtype AcrossSpaceAndTime = AcrossSpaceAndTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

acrossSpaceAndTime :: TreacheryCard AcrossSpaceAndTime
acrossSpaceAndTime = treachery AcrossSpaceAndTime Cards.acrossSpaceAndTime

instance RunMessage AcrossSpaceAndTime where
  runMessage msg t@(AcrossSpaceAndTime attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ DiscardTopOfDeck iid 3 (toSource attrs) Nothing
      pure t
    _ -> AcrossSpaceAndTime <$> runMessage msg attrs
