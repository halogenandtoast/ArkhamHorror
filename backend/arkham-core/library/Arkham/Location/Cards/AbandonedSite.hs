module Arkham.Location.Cards.AbandonedSite
  ( abandonedSite
  , AbandonedSite(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype AbandonedSite = AbandonedSite LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

abandonedSite :: LocationCard AbandonedSite
abandonedSite =
  symbolLabel $ location AbandonedSite Cards.abandonedSite 0 (PerPlayer 1)

instance HasModifiersFor AbandonedSite where
  getModifiersFor target (AbandonedSite attrs) | isTarget attrs target = do
    n <- getCurrentDepth
    pure $ toModifiers attrs [ShroudModifier n]
  getModifiersFor _ _ = pure []

instance RunMessage AbandonedSite where
  runMessage msg (AbandonedSite attrs) = AbandonedSite <$> runMessage msg attrs
