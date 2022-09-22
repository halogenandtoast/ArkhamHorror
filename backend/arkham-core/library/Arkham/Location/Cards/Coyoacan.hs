module Arkham.Location.Cards.Coyoacan
  ( coyoacan
  , Coyoacan(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Coyoacan = Coyoacan LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coyoacan :: LocationCard Coyoacan
coyoacan = locationWith Coyoacan Cards.coyoacan 2 (Static 0) (labelL .~ "star")

instance HasAbilities Coyoacan where
  getAbilities (Coyoacan attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage Coyoacan where
  runMessage msg (Coyoacan attrs) = Coyoacan <$> runMessage msg attrs
