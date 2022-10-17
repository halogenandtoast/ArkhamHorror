module Arkham.Location.Cards.HallOfIdolatry
  ( hallOfIdolatry
  , HallOfIdolatry(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype HallOfIdolatry = HallOfIdolatry LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfIdolatry :: LocationCard HallOfIdolatry
hallOfIdolatry = location HallOfIdolatry Cards.hallOfIdolatry 3 (PerPlayer 2)

instance HasAbilities HallOfIdolatry where
  getAbilities (HallOfIdolatry attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage HallOfIdolatry where
  runMessage msg (HallOfIdolatry attrs) =
    HallOfIdolatry <$> runMessage msg attrs
