module Arkham.Location.Cards.SelangorClub (selangorClub) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SelangorClub = SelangorClub LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

selangorClub :: LocationCard SelangorClub
selangorClub = symbolLabel $ location SelangorClub Cards.selangorClub 0 (Static 0)

instance HasAbilities SelangorClub where
  getAbilities (SelangorClub a) =
    extendRevealed a []

instance RunMessage SelangorClub where
  runMessage msg (SelangorClub attrs) = runQueueT $ case msg of
    _ -> SelangorClub <$> liftRunMessage msg attrs
