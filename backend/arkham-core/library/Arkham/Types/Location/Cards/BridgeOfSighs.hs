module Arkham.Types.Location.Cards.BridgeOfSighs
  ( bridgeOfSighs
  , BridgeOfSighs(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype BridgeOfSighs = BridgeOfSighs LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bridgeOfSighs :: LocationCard BridgeOfSighs
bridgeOfSighs = locationWith
  BridgeOfSighs
  Cards.bridgeOfSighs
  0
  (Static 0)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env BridgeOfSighs

instance ActionRunner env => HasActions env BridgeOfSighs where
  getActions iid window (BridgeOfSighs attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env BridgeOfSighs where
  runMessage msg (BridgeOfSighs attrs) = BridgeOfSighs <$> runMessage msg attrs
