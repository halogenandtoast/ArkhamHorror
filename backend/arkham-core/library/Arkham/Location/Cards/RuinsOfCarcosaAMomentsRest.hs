module Arkham.Location.Cards.RuinsOfCarcosaAMomentsRest
  ( ruinsOfCarcosaAMomentsRest
  , RuinsOfCarcosaAMomentsRest(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import qualified Arkham.Location.Cards as Cards
import Arkham.Location.Runner

newtype RuinsOfCarcosaAMomentsRest = RuinsOfCarcosaAMomentsRest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfCarcosaAMomentsRest :: LocationCard RuinsOfCarcosaAMomentsRest
ruinsOfCarcosaAMomentsRest = location
  RuinsOfCarcosaAMomentsRest
  Cards.ruinsOfCarcosaAMomentsRest
  2
  (PerPlayer 1)
  Triangle
  [Square, Equals, Star]

instance HasAbilities RuinsOfCarcosaAMomentsRest where
  getAbilities (RuinsOfCarcosaAMomentsRest attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage RuinsOfCarcosaAMomentsRest where
  runMessage msg (RuinsOfCarcosaAMomentsRest attrs) =
    RuinsOfCarcosaAMomentsRest <$> runMessage msg attrs
