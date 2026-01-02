module Arkham.Location.Cards.ValeSchoolhouseNight (valeSchoolhouseNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ValeSchoolhouseNight = ValeSchoolhouseNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeSchoolhouseNight :: LocationCard ValeSchoolhouseNight
valeSchoolhouseNight = symbolLabel $ location ValeSchoolhouseNight Cards.valeSchoolhouseNight 0 (Static 0)

instance HasAbilities ValeSchoolhouseNight where
  getAbilities (ValeSchoolhouseNight a) =
    extendRevealed a []

instance RunMessage ValeSchoolhouseNight where
  runMessage msg (ValeSchoolhouseNight attrs) = runQueueT $ case msg of
    _ -> ValeSchoolhouseNight <$> liftRunMessage msg attrs
