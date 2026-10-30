-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Acts.AFamilyAffair (aFamilyAffair) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.NightOfVespers.CardDefs.Acts qualified as Cards

newtype AFamilyAffair = AFamilyAffair ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

aFamilyAffair :: ActCard AFamilyAffair
aFamilyAffair = act (2, A) AFamilyAffair Cards.aFamilyAffair Nothing

instance RunMessage AFamilyAffair where
  runMessage msg (AFamilyAffair attrs) = AFamilyAffair <$> runMessage msg attrs
