module Arkham.Location.Cards.BridgeOverNKai (
  bridgeOverNKai,
  BridgeOverNKai (..),
) where

import Arkham.Prelude

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype BridgeOverNKai = BridgeOverNKai LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bridgeOverNKai :: LocationCard BridgeOverNKai
bridgeOverNKai =
  symbolLabel $ location BridgeOverNKai Cards.bridgeOverNKai 2 (PerPlayer 1)

instance HasModifiersFor BridgeOverNKai where
  getModifiersFor (BridgeOverNKai a) = do
    n <- getVengeanceInVictoryDisplay
    modifySelf a [ShroudModifier n]

instance RunMessage BridgeOverNKai where
  runMessage msg (BridgeOverNKai attrs) =
    BridgeOverNKai <$> runMessage msg attrs
