module Arkham.Location.Cards.AbandonedSite
  ( abandonedSite
  , AbandonedSite(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype AbandonedSite = AbandonedSite LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedSite :: LocationCard AbandonedSite
abandonedSite =
  symbolLabel $ location AbandonedSite Cards.abandonedSite 0 (PerPlayer 1)

instance HasAbilities AbandonedSite where
  getAbilities (AbandonedSite attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage AbandonedSite where
  runMessage msg (AbandonedSite attrs) =
    AbandonedSite <$> runMessage msg attrs
