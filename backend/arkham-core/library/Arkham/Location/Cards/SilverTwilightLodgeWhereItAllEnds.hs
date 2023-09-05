module Arkham.Location.Cards.SilverTwilightLodgeWhereItAllEnds (
  silverTwilightLodgeWhereItAllEnds,
  SilverTwilightLodgeWhereItAllEnds (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SilverTwilightLodgeWhereItAllEnds = SilverTwilightLodgeWhereItAllEnds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightLodgeWhereItAllEnds :: LocationCard SilverTwilightLodgeWhereItAllEnds
silverTwilightLodgeWhereItAllEnds = location SilverTwilightLodgeWhereItAllEnds Cards.silverTwilightLodgeWhereItAllEnds 2 (Static 0)

instance HasAbilities SilverTwilightLodgeWhereItAllEnds where
  getAbilities (SilverTwilightLodgeWhereItAllEnds attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage SilverTwilightLodgeWhereItAllEnds where
  runMessage msg (SilverTwilightLodgeWhereItAllEnds attrs) =
    SilverTwilightLodgeWhereItAllEnds <$> runMessage msg attrs
