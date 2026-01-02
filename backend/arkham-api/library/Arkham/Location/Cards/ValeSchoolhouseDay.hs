module Arkham.Location.Cards.ValeSchoolhouseDay (valeSchoolhouseDay) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ValeSchoolhouseDay = ValeSchoolhouseDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeSchoolhouseDay :: LocationCard ValeSchoolhouseDay
valeSchoolhouseDay = symbolLabel $ location ValeSchoolhouseDay Cards.valeSchoolhouseDay 0 (Static 0)

instance HasAbilities ValeSchoolhouseDay where
  getAbilities (ValeSchoolhouseDay a) =
    extendRevealed a []

instance RunMessage ValeSchoolhouseDay where
  runMessage msg (ValeSchoolhouseDay attrs) = runQueueT $ case msg of
    _ -> ValeSchoolhouseDay <$> liftRunMessage msg attrs
