module Arkham.Location.Cards.GrandBazaarDarkenedAlley (grandBazaarDarkenedAlley) where

import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GrandBazaarDarkenedAlley = GrandBazaarDarkenedAlley LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

grandBazaarDarkenedAlley :: LocationCard GrandBazaarDarkenedAlley
grandBazaarDarkenedAlley =
  locationWith GrandBazaarDarkenedAlley Cards.grandBazaarDarkenedAlley 2 (Static 1) connectsToAdjacent

instance HasModifiersFor GrandBazaarDarkenedAlley where
  getModifiersFor (GrandBazaarDarkenedAlley a) = modifySelect a (ConcealedCardAt $ be a) [EnemyEvade 2, EnemyFight 2]

instance RunMessage GrandBazaarDarkenedAlley where
  runMessage msg (GrandBazaarDarkenedAlley attrs) = GrandBazaarDarkenedAlley <$> runMessage msg attrs
