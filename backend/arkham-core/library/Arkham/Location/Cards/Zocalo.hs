module Arkham.Location.Cards.Zocalo
  ( zocalo
  , Zocalo(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Zocalo = Zocalo LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zocalo :: LocationCard Zocalo
zocalo = locationWith Zocalo Cards.zocalo 3 (Static 0) (labelL .~ "diamond")

instance HasAbilities Zocalo where
  getAbilities (Zocalo attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage Zocalo where
  runMessage msg (Zocalo attrs) = Zocalo <$> runMessage msg attrs
