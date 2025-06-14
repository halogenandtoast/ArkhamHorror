module Arkham.Location.Cards.Zocalo (zocalo) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Zocalo = Zocalo LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zocalo :: LocationCard Zocalo
zocalo = symbolLabel $ location Zocalo Cards.zocalo 3 (Static 0)

instance HasAbilities Zocalo where
  getAbilities (Zocalo a) = extendRevealed1 a $ restricted a 1 Here $ exploreAction $ DiscardCombinedCost 5

instance RunMessage Zocalo where
  runMessage msg l@(Zocalo attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Explore iid (attrs.ability 1) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> Zocalo <$> liftRunMessage msg attrs
