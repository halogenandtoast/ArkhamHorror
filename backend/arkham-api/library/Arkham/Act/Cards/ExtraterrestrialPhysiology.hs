module Arkham.Act.Cards.ExtraterrestrialPhysiology (extraterrestrialPhysiology) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ExtraterrestrialPhysiology = ExtraterrestrialPhysiology ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extraterrestrialPhysiology :: ActCard ExtraterrestrialPhysiology
extraterrestrialPhysiology = act (2, A) ExtraterrestrialPhysiology Cards.extraterrestrialPhysiology Nothing

instance RunMessage ExtraterrestrialPhysiology where
  runMessage msg (ExtraterrestrialPhysiology attrs) = ExtraterrestrialPhysiology <$> runMessage msg attrs
