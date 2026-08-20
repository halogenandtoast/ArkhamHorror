module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.ErwinBridgeDawn (erwinBridgeDawn) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype ErwinBridgeDawn = ErwinBridgeDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erwinBridgeDawn :: LocationCard ErwinBridgeDawn
erwinBridgeDawn = symbolLabel $ location ErwinBridgeDawn Cards.erwinBridgeDawn 1 (Static 0)

instance HasAbilities ErwinBridgeDawn where
  getAbilities (ErwinBridgeDawn a) =
    extendRevealed a []

instance RunMessage ErwinBridgeDawn where
  runMessage msg (ErwinBridgeDawn attrs) = runQueueT $ case msg of
    _ -> ErwinBridgeDawn <$> liftRunMessage msg attrs
