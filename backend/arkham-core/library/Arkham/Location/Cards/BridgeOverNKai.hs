module Arkham.Location.Cards.BridgeOverNKai
  ( bridgeOverNKai
  , BridgeOverNKai(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype BridgeOverNKai = BridgeOverNKai LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bridgeOverNKai :: LocationCard BridgeOverNKai
bridgeOverNKai =
  symbolLabel $ location BridgeOverNKai Cards.bridgeOverNKai 2 (PerPlayer 1)

instance HasAbilities BridgeOverNKai where
  getAbilities (BridgeOverNKai attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BridgeOverNKai where
  runMessage msg (BridgeOverNKai attrs) =
    BridgeOverNKai <$> runMessage msg attrs
