module Arkham.Location.Cards.Lounge
  ( lounge
  , Lounge(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Lounge = Lounge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lounge :: LocationCard Lounge
lounge = location Lounge Cards.lounge 2 (PerPlayer 2)

instance HasAbilities Lounge where
  getAbilities (Lounge attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Lounge where
  runMessage msg (Lounge attrs) =
    Lounge <$> runMessage msg attrs
