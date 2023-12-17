module Arkham.Location.Cards.Kadatheron
  ( kadatheron
  , Kadatheron(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Kadatheron = Kadatheron LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kadatheron :: LocationCard Kadatheron
kadatheron = location Kadatheron Cards.kadatheron 5 (PerPlayer 1)

instance HasAbilities Kadatheron where
  getAbilities (Kadatheron attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Kadatheron where
  runMessage msg (Kadatheron attrs) =
    Kadatheron <$> runMessage msg attrs
