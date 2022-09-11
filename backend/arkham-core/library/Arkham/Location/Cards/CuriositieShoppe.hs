module Arkham.Location.Cards.CuriositieShoppe
  ( curiositieShoppe
  , CuriositieShoppe(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CuriositieShoppe = CuriositieShoppe LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curiositieShoppe :: LocationCard CuriositieShoppe
curiositieShoppe =
  location CuriositieShoppe Cards.curiositieShoppe 2 (PerPlayer 2)

instance HasAbilities CuriositieShoppe where
  getAbilities (CuriositieShoppe attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CuriositieShoppe where
  runMessage msg (CuriositieShoppe attrs) =
    CuriositieShoppe <$> runMessage msg attrs
