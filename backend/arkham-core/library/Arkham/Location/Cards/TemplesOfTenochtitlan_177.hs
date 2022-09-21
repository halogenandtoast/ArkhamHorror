module Arkham.Location.Cards.TemplesOfTenochtitlan_177
  ( templesOfTenochtitlan_177
  , TemplesOfTenochtitlan_177(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TemplesOfTenochtitlan_177 = TemplesOfTenochtitlan_177 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templesOfTenochtitlan_177 :: LocationCard TemplesOfTenochtitlan_177
templesOfTenochtitlan_177 = location
  TemplesOfTenochtitlan_177
  Cards.templesOfTenochtitlan_177
  2
  (PerPlayer 2)

instance HasAbilities TemplesOfTenochtitlan_177 where
  getAbilities (TemplesOfTenochtitlan_177 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TemplesOfTenochtitlan_177 where
  runMessage msg (TemplesOfTenochtitlan_177 attrs) =
    TemplesOfTenochtitlan_177 <$> runMessage msg attrs
