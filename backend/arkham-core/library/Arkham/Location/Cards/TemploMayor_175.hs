module Arkham.Location.Cards.TemploMayor_175
  ( temploMayor_175
  , TemploMayor_175(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TemploMayor_175 = TemploMayor_175 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temploMayor_175 :: LocationCard TemploMayor_175
temploMayor_175 = locationWith
  TemploMayor_175
  Cards.temploMayor_175
  2
  (PerPlayer 2)
  (labelL .~ "circle")

instance HasAbilities TemploMayor_175 where
  getAbilities (TemploMayor_175 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TemploMayor_175 where
  runMessage msg (TemploMayor_175 attrs) =
    TemploMayor_175 <$> runMessage msg attrs
