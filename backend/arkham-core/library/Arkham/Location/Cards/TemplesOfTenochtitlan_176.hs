module Arkham.Location.Cards.TemplesOfTenochtitlan_176
  ( templesOfTenochtitlan_176
  , TemplesOfTenochtitlan_176(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TemplesOfTenochtitlan_176 = TemplesOfTenochtitlan_176 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templesOfTenochtitlan_176 :: LocationCard TemplesOfTenochtitlan_176
templesOfTenochtitlan_176 = locationWith
  TemplesOfTenochtitlan_176
  Cards.templesOfTenochtitlan_176
  3
  (PerPlayer 1)
  (labelL .~ "square")

instance HasAbilities TemplesOfTenochtitlan_176 where
  getAbilities (TemplesOfTenochtitlan_176 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TemplesOfTenochtitlan_176 where
  runMessage msg (TemplesOfTenochtitlan_176 attrs) =
    TemplesOfTenochtitlan_176 <$> runMessage msg attrs
