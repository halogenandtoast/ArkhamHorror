module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.ErwinBridgeDusk (erwinBridgeDusk) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype ErwinBridgeDusk = ErwinBridgeDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erwinBridgeDusk :: LocationCard ErwinBridgeDusk
erwinBridgeDusk = symbolLabel $ location ErwinBridgeDusk Cards.erwinBridgeDusk 1 (Static 0)

instance HasAbilities ErwinBridgeDusk where
  getAbilities (ErwinBridgeDusk a) =
    extendRevealed a []

instance RunMessage ErwinBridgeDusk where
  runMessage msg (ErwinBridgeDusk attrs) = runQueueT $ case msg of
    _ -> ErwinBridgeDusk <$> liftRunMessage msg attrs
