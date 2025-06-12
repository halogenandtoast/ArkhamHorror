module Arkham.Location.Cards.VastPassages (vastPassages) where

import Arkham.Action qualified as Action
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype VastPassages = VastPassages LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

vastPassages :: LocationCard VastPassages
vastPassages = symbolLabel $ location VastPassages Cards.vastPassages 2 (PerPlayer 1)

instance HasModifiersFor VastPassages where
  getModifiersFor (VastPassages a) = do
    ok <- selectNone $ InvestigatorWithSupply Binoculars <> investigatorAt a
    modifySelectWhen a ok Anyone [AdditionalActionCostOf (IsAction Action.Explore) 1]

instance RunMessage VastPassages where
  runMessage msg (VastPassages attrs) = VastPassages <$> runMessage msg attrs
