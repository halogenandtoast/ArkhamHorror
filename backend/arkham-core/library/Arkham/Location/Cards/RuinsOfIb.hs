module Arkham.Location.Cards.RuinsOfIb
  ( ruinsOfIb
  , RuinsOfIb(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype RuinsOfIb = RuinsOfIb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfIb :: LocationCard RuinsOfIb
ruinsOfIb = location RuinsOfIb Cards.ruinsOfIb 1 (PerPlayer 1)

instance HasAbilities RuinsOfIb where
  getAbilities (RuinsOfIb attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage RuinsOfIb where
  runMessage msg (RuinsOfIb attrs) =
    RuinsOfIb <$> runMessage msg attrs
