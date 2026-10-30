-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Acts.ThusWithAKiss (thusWithAKiss) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.NightOfVespers.CardDefs.Acts qualified as Cards

newtype ThusWithAKiss = ThusWithAKiss ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thusWithAKiss :: ActCard ThusWithAKiss
thusWithAKiss = act (1, A) ThusWithAKiss Cards.thusWithAKiss Nothing

instance RunMessage ThusWithAKiss where
  runMessage msg (ThusWithAKiss attrs) = ThusWithAKiss <$> runMessage msg attrs
