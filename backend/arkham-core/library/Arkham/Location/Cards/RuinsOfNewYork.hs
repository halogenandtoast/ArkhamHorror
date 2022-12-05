module Arkham.Location.Cards.RuinsOfNewYork
  ( ruinsOfNewYork
  , RuinsOfNewYork(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype RuinsOfNewYork = RuinsOfNewYork LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfNewYork :: LocationCard RuinsOfNewYork
ruinsOfNewYork = location RuinsOfNewYork Cards.ruinsOfNewYork 1 (Static 3)

instance HasAbilities RuinsOfNewYork where
  getAbilities (RuinsOfNewYork attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage RuinsOfNewYork where
  runMessage msg (RuinsOfNewYork attrs) =
    RuinsOfNewYork <$> runMessage msg attrs
