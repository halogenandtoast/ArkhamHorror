-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Acts.IntoTheFire (intoTheFire) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.NightOfVespers.CardDefs.Acts qualified as Cards

newtype IntoTheFire = IntoTheFire ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

intoTheFire :: ActCard IntoTheFire
intoTheFire = act (3, A) IntoTheFire Cards.intoTheFire Nothing

instance RunMessage IntoTheFire where
  runMessage msg (IntoTheFire attrs) = IntoTheFire <$> runMessage msg attrs
