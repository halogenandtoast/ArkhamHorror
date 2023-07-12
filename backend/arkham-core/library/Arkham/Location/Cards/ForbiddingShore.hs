module Arkham.Location.Cards.ForbiddingShore
  ( forbiddingShore
  , ForbiddingShore(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ForbiddingShore = ForbiddingShore LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddingShore :: LocationCard ForbiddingShore
forbiddingShore = location ForbiddingShore Cards.forbiddingShore 3 (PerPlayer 1)

instance HasAbilities ForbiddingShore where
  getAbilities (ForbiddingShore attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage ForbiddingShore where
  runMessage msg (ForbiddingShore attrs) =
    ForbiddingShore <$> runMessage msg attrs
