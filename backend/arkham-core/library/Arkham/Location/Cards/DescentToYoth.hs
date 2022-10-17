module Arkham.Location.Cards.DescentToYoth
  ( descentToYoth
  , DescentToYoth(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype DescentToYoth = DescentToYoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descentToYoth :: LocationCard DescentToYoth
descentToYoth = location DescentToYoth Cards.descentToYoth 3 (Static 0)

instance HasAbilities DescentToYoth where
  getAbilities (DescentToYoth attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage DescentToYoth where
  runMessage msg (DescentToYoth attrs) =
    DescentToYoth <$> runMessage msg attrs
