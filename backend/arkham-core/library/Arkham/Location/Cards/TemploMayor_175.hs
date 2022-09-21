module Arkham.Location.Cards.TemploMayor_175
  ( temploMayor_175
  , TemploMayor_175(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype TemploMayor_175 = TemploMayor_175 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temploMayor_175 :: LocationCard TemploMayor_175
temploMayor_175 = location TemploMayor_175 Cards.temploMayor_175 2 (PerPlayer 2)

instance HasAbilities TemploMayor_175 where
  getAbilities (TemploMayor_175 attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TemploMayor_175 where
  runMessage msg (TemploMayor_175 attrs) =
    TemploMayor_175 <$> runMessage msg attrs
