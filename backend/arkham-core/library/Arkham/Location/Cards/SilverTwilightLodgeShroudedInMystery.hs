module Arkham.Location.Cards.SilverTwilightLodgeShroudedInMystery
  ( silverTwilightLodgeShroudedInMystery
  , SilverTwilightLodgeShroudedInMystery(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SilverTwilightLodgeShroudedInMystery = SilverTwilightLodgeShroudedInMystery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightLodgeShroudedInMystery :: LocationCard SilverTwilightLodgeShroudedInMystery
silverTwilightLodgeShroudedInMystery = location SilverTwilightLodgeShroudedInMystery Cards.silverTwilightLodgeShroudedInMystery 4 (PerPlayer 1)

instance HasAbilities SilverTwilightLodgeShroudedInMystery where
  getAbilities (SilverTwilightLodgeShroudedInMystery attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage SilverTwilightLodgeShroudedInMystery where
  runMessage msg (SilverTwilightLodgeShroudedInMystery attrs) =
    SilverTwilightLodgeShroudedInMystery <$> runMessage msg attrs
