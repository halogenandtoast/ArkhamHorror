module Arkham.Location.Cards.TheScarletKeys.DealingsInTheDark.GrandBazaarPublicBaths (grandBazaarPublicBaths) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.CardDefs.TheScarletKeys.DealingsInTheDark qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GrandBazaarPublicBaths = GrandBazaarPublicBaths LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

grandBazaarPublicBaths :: LocationCard GrandBazaarPublicBaths
grandBazaarPublicBaths =
  locationWith GrandBazaarPublicBaths Cards.grandBazaarPublicBaths 2 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor GrandBazaarPublicBaths where
  getModifiersFor (GrandBazaarPublicBaths a) = do
    modifySelect a (AssetControlledBy (InvestigatorAt $ be a) <> #weapon) [Blank]

instance RunMessage GrandBazaarPublicBaths where
  runMessage msg (GrandBazaarPublicBaths attrs) = GrandBazaarPublicBaths <$> runMessage msg attrs
