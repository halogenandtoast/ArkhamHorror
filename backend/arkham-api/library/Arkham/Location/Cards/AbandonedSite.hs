module Arkham.Location.Cards.AbandonedSite (abandonedSite, AbandonedSite (..)) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype AbandonedSite = AbandonedSite LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

abandonedSite :: LocationCard AbandonedSite
abandonedSite = symbolLabel $ location AbandonedSite Cards.abandonedSite 0 (PerPlayer 1)

instance HasModifiersFor AbandonedSite where
  getModifiersFor (AbandonedSite attrs) = do
    n <- getCurrentDepth
    modifySelf attrs [ShroudModifier n]

instance RunMessage AbandonedSite where
  runMessage msg (AbandonedSite attrs) = AbandonedSite <$> runMessage msg attrs
