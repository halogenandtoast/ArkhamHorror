module Arkham.Location.Cards.BrightCanyon
  ( brightCanyon
  , BrightCanyon(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype BrightCanyon = BrightCanyon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brightCanyon :: LocationCard BrightCanyon
brightCanyon =
  symbolLabel $ location BrightCanyon Cards.brightCanyon 2 (PerPlayer 2)

instance HasAbilities BrightCanyon where
  getAbilities (BrightCanyon attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BrightCanyon where
  runMessage msg (BrightCanyon attrs) = BrightCanyon <$> runMessage msg attrs
