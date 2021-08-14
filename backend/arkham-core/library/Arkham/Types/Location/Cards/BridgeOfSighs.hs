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
import Arkham.Types.Message

newtype BridgeOfSighs = BridgeOfSighs LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bridgeOfSighs :: LocationCard BridgeOfSighs
bridgeOfSighs = locationWith
  BridgeOfSighs
  Cards.bridgeOfSighs
  1
  (Static 2)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env BridgeOfSighs

instance ActionRunner env => HasAbilities env BridgeOfSighs where
  getAbilities iid window (BridgeOfSighs attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env BridgeOfSighs where
  runMessage msg l@(BridgeOfSighs attrs) = case msg of
    MoveFrom iid lid | lid == toId attrs ->
      l <$ push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1)
    _ -> BridgeOfSighs <$> runMessage msg attrs
