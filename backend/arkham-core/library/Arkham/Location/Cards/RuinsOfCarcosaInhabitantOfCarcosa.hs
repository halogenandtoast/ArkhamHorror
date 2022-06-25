module Arkham.Location.Cards.RuinsOfCarcosaInhabitantOfCarcosa
  ( ruinsOfCarcosaInhabitantOfCarcosa
  , RuinsOfCarcosaInhabitantOfCarcosa(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype RuinsOfCarcosaInhabitantOfCarcosa = RuinsOfCarcosaInhabitantOfCarcosa LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfCarcosaInhabitantOfCarcosa
  :: LocationCard RuinsOfCarcosaInhabitantOfCarcosa
ruinsOfCarcosaInhabitantOfCarcosa = location
  RuinsOfCarcosaInhabitantOfCarcosa
  Cards.ruinsOfCarcosaInhabitantOfCarcosa
  2
  (PerPlayer 1)
  Triangle
  [Square, Equals, Star]

instance HasAbilities RuinsOfCarcosaInhabitantOfCarcosa where
  getAbilities (RuinsOfCarcosaInhabitantOfCarcosa attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage RuinsOfCarcosaInhabitantOfCarcosa where
  runMessage msg (RuinsOfCarcosaInhabitantOfCarcosa attrs) =
    RuinsOfCarcosaInhabitantOfCarcosa <$> runMessage msg attrs
