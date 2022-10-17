module Arkham.Location.Cards.DarkHollow
  ( darkHollow
  , DarkHollow(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype DarkHollow = DarkHollow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkHollow :: LocationCard DarkHollow
darkHollow = location DarkHollow Cards.darkHollow 3 (PerPlayer 1)

instance HasAbilities DarkHollow where
  getAbilities (DarkHollow attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage DarkHollow where
  runMessage msg (DarkHollow attrs) =
    DarkHollow <$> runMessage msg attrs
