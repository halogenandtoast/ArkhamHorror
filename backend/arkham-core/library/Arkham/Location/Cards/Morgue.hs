module Arkham.Location.Cards.Morgue
  ( morgue
  , Morgue(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Morgue = Morgue LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

morgue :: LocationCard Morgue
morgue = location Morgue Cards.morgue 5 (PerPlayer 1)

instance HasAbilities Morgue where
  getAbilities (Morgue attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Morgue where
  runMessage msg (Morgue attrs) =
    Morgue <$> runMessage msg attrs
