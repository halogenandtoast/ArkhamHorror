module Arkham.Location.Cards.VastPassages (
  vastPassages,
  VastPassages (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype VastPassages = VastPassages LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

vastPassages :: LocationCard VastPassages
vastPassages = location VastPassages Cards.vastPassages 2 (PerPlayer 1)

instance HasModifiersFor VastPassages where
  getModifiersFor (InvestigatorTarget iid) (VastPassages attrs) = do
    here <- iid `isAt` attrs
    withBinoculars <- getHasSupply iid Binoculars
    pure $ toModifiers attrs [ActionCostOf (IsAction Action.Explore) 1 | here, not withBinoculars]
  getModifiersFor _ _ = pure []

instance RunMessage VastPassages where
  runMessage msg (VastPassages attrs) = VastPassages <$> runMessage msg attrs
