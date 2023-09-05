module Arkham.Location.Cards.HangmansHillShroudedInMystery
  ( hangmansHillShroudedInMystery
  , HangmansHillShroudedInMystery(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HangmansHillShroudedInMystery = HangmansHillShroudedInMystery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangmansHillShroudedInMystery :: LocationCard HangmansHillShroudedInMystery
hangmansHillShroudedInMystery = location HangmansHillShroudedInMystery Cards.hangmansHillShroudedInMystery 4 (PerPlayer 1)

instance HasAbilities HangmansHillShroudedInMystery where
  getAbilities (HangmansHillShroudedInMystery attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage HangmansHillShroudedInMystery where
  runMessage msg (HangmansHillShroudedInMystery attrs) =
    HangmansHillShroudedInMystery <$> runMessage msg attrs
