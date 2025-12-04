module Arkham.Location.Cards.SelangorClubPadang (selangorClubPadang) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SelangorClubPadang = SelangorClubPadang LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

selangorClubPadang :: LocationCard SelangorClubPadang
selangorClubPadang = symbolLabel $ location SelangorClubPadang Cards.selangorClubPadang 0 (Static 0)

instance HasAbilities SelangorClubPadang where
  getAbilities (SelangorClubPadang a) =
    extendRevealed a []

instance RunMessage SelangorClubPadang where
  runMessage msg (SelangorClubPadang attrs) = runQueueT $ case msg of
    _ -> SelangorClubPadang <$> liftRunMessage msg attrs
