module Arkham.Act.Cards.ObtainingTheDevice
  ( ObtainingTheDevice(..)
  , obtainingTheDevice
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype ObtainingTheDevice = ObtainingTheDevice ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

obtainingTheDevice :: ActCard ObtainingTheDevice
obtainingTheDevice = act (2, A) ObtainingTheDevice Cards.obtainingTheDevice Nothing

instance RunMessage ObtainingTheDevice where
  runMessage msg (ObtainingTheDevice attrs) = ObtainingTheDevice <$> runMessage msg attrs
