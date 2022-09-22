module Arkham.Location.Cards.TemploMayor_174
  ( temploMayor_174
  , TemploMayor_174(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TemploMayor_174 = TemploMayor_174 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temploMayor_174 :: LocationCard TemploMayor_174
temploMayor_174 = locationWith
  TemploMayor_174
  Cards.temploMayor_174
  4
  (PerPlayer 1)
  (labelL .~ "circle")

instance HasAbilities TemploMayor_174 where
  getAbilities (TemploMayor_174 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TemploMayor_174 where
  runMessage msg (TemploMayor_174 attrs) =
    TemploMayor_174 <$> runMessage msg attrs
