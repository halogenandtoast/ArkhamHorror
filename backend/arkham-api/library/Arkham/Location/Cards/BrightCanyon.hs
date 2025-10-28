module Arkham.Location.Cards.BrightCanyon (brightCanyon) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Discover
import Arkham.GameValue
import Arkham.Helpers.Window (enters)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype BrightCanyon = BrightCanyon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brightCanyon :: LocationCard BrightCanyon
brightCanyon = symbolLabel $ location BrightCanyon Cards.brightCanyon 2 (PerPlayer 2)

instance HasAbilities BrightCanyon where
  getAbilities (BrightCanyon attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 (you $ have Treacheries.poisoned) $ forced $ enters #after You attrs
      , groupLimit PerDepthLevel
          $ restricted attrs 2 (Here <> cluesOnThis 1 <> you (have Binoculars)) actionAbility
      ]

instance RunMessage BrightCanyon where
  runMessage msg l@(BrightCanyon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (toAbilitySource attrs 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discoverAt NotInvestigate iid (attrs.ability 2) 2 attrs
      pure l
    _ -> BrightCanyon <$> liftRunMessage msg attrs
