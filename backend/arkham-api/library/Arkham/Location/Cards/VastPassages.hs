module Arkham.Location.Cards.VastPassages (vastPassages, VastPassages (..)) where

import Arkham.Action qualified as Action
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype VastPassages = VastPassages LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

vastPassages :: LocationCard VastPassages
vastPassages = location VastPassages Cards.vastPassages 2 (PerPlayer 1)

instance HasModifiersFor VastPassages where
  getModifiersFor (VastPassages a) = do
    modifySelect
      a
      (not_ (InvestigatorWithSupply Binoculars) <> investigatorAt a)
      [AdditionalActionCostOf (IsAction Action.Explore) 1]

instance RunMessage VastPassages where
  runMessage msg (VastPassages attrs) = VastPassages <$> runMessage msg attrs
