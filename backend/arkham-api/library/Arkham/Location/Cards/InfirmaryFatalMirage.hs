module Arkham.Location.Cards.InfirmaryFatalMirage (infirmaryFatalMirage) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype InfirmaryFatalMirage = InfirmaryFatalMirage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infirmaryFatalMirage :: LocationCard InfirmaryFatalMirage
infirmaryFatalMirage = location InfirmaryFatalMirage Cards.infirmaryFatalMirage 4 (PerPlayer 2)

instance HasAbilities InfirmaryFatalMirage where
  getAbilities (InfirmaryFatalMirage attrs) =
    extendRevealed attrs []

instance RunMessage InfirmaryFatalMirage where
  runMessage msg (InfirmaryFatalMirage attrs) = runQueueT $ case msg of
    _ -> InfirmaryFatalMirage <$> liftRunMessage msg attrs
