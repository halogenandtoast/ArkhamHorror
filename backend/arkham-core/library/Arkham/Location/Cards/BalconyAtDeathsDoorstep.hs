module Arkham.Location.Cards.BalconyAtDeathsDoorstep
  ( balconyAtDeathsDoorstep
  , BalconyAtDeathsDoorstep(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype BalconyAtDeathsDoorstep = BalconyAtDeathsDoorstep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balconyAtDeathsDoorstep :: LocationCard BalconyAtDeathsDoorstep
balconyAtDeathsDoorstep = location BalconyAtDeathsDoorstep Cards.balconyAtDeathsDoorstep 1 (Static 0)

instance HasAbilities BalconyAtDeathsDoorstep where
  getAbilities (BalconyAtDeathsDoorstep attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BalconyAtDeathsDoorstep where
  runMessage msg (BalconyAtDeathsDoorstep attrs) =
    BalconyAtDeathsDoorstep <$> runMessage msg attrs
