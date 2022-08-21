module Arkham.Location.Cards.AncientHall
  ( ancientHall
  , AncientHall(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype AncientHall = AncientHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientHall :: LocationCard AncientHall
ancientHall = location AncientHall Cards.ancientHall 3 (PerPlayer 2)

instance HasAbilities AncientHall where
  getAbilities (AncientHall attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage AncientHall where
  runMessage msg (AncientHall attrs) =
    AncientHall <$> runMessage msg attrs
