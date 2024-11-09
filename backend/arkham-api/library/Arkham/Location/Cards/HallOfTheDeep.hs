module Arkham.Location.Cards.HallOfTheDeep (hallOfTheDeep, HallOfTheDeep (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HallOfTheDeep = HallOfTheDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfTheDeep :: LocationCard HallOfTheDeep
hallOfTheDeep = location HallOfTheDeep Cards.hallOfTheDeep 3 (PerPlayer 1)

instance HasAbilities HallOfTheDeep where
  getAbilities (HallOfTheDeep a) =
    extendRevealed
      a
      [ restricted a 1 Here $ actionAbilityWithCost (SpendKeyCost RedKey)
      , mkAbility a 2 $ forced $ RevealLocation #after Anyone (be a)
      ]

instance RunMessage HallOfTheDeep where
  runMessage msg l@(HallOfTheDeep attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.yhanthleiStatueMysteriousRelic
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      increaseThisFloodLevel attrs
      pure l
    _ -> HallOfTheDeep <$> liftRunMessage msg attrs
