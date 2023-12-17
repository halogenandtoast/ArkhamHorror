module Arkham.Location.Cards.Sarnath
  ( sarnath
  , Sarnath(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Sarnath = Sarnath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sarnath :: LocationCard Sarnath
sarnath = location Sarnath Cards.sarnath 3 (PerPlayer 1)

instance HasAbilities Sarnath where
  getAbilities (Sarnath attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Sarnath where
  runMessage msg (Sarnath attrs) =
    Sarnath <$> runMessage msg attrs
