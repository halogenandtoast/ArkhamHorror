module Arkham.Location.Cards.DarkHollow (darkHollow) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Helpers.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DarkHollow = DarkHollow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkHollow :: LocationCard DarkHollow
darkHollow = symbolLabel $ location DarkHollow Cards.darkHollow 3 (PerPlayer 1)

instance HasAbilities DarkHollow where
  getAbilities (DarkHollow a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ PutLocationIntoPlay #after Anyone (be a)

instance RunMessage DarkHollow where
  runMessage msg l@(DarkHollow attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      anyHasMap <- getAnyHasSupply Map
      unless anyHasMap $ do
        n <- perPlayer 1
        placeClues attrs attrs n
      pure l
    _ -> DarkHollow <$> liftRunMessage msg attrs
