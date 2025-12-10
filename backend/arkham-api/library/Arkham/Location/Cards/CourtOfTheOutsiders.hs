module Arkham.Location.Cards.CourtOfTheOutsiders (courtOfTheOutsiders) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CourtOfTheOutsiders = CourtOfTheOutsiders LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtOfTheOutsiders :: LocationCard CourtOfTheOutsiders
courtOfTheOutsiders =
  symbolLabel
    $ locationWith CourtOfTheOutsiders Cards.courtOfTheOutsiders 2 (PerPlayer 2) connectsToAdjacent

instance HasAbilities CourtOfTheOutsiders where
  getAbilities (CourtOfTheOutsiders a) =
    extendRevealed a []

instance RunMessage CourtOfTheOutsiders where
  runMessage msg (CourtOfTheOutsiders attrs) = runQueueT $ case msg of
    _ -> CourtOfTheOutsiders <$> liftRunMessage msg attrs
