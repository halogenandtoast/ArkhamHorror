module Arkham.Location.Cards.RuinsOfCarcosaTheCoffin
  ( ruinsOfCarcosaTheCoffin
  , RuinsOfCarcosaTheCoffin(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype RuinsOfCarcosaTheCoffin = RuinsOfCarcosaTheCoffin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfCarcosaTheCoffin :: LocationCard RuinsOfCarcosaTheCoffin
ruinsOfCarcosaTheCoffin = location
  RuinsOfCarcosaTheCoffin
  Cards.ruinsOfCarcosaTheCoffin
  2
  (PerPlayer 1)
  Triangle
  [Square, Equals, Star]

instance HasAbilities RuinsOfCarcosaTheCoffin where
  getAbilities (RuinsOfCarcosaTheCoffin attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage RuinsOfCarcosaTheCoffin where
  runMessage msg (RuinsOfCarcosaTheCoffin attrs) =
    RuinsOfCarcosaTheCoffin <$> runMessage msg attrs
