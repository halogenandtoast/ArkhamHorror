module Arkham.Location.Cards.TheCrossroadsDay (theCrossroadsDay) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher.Base
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype TheCrossroadsDay = TheCrossroadsDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCrossroadsDay :: LocationCard TheCrossroadsDay
theCrossroadsDay = symbolLabel $ location TheCrossroadsDay Cards.theCrossroadsDay 2 (Static 0)

instance HasAbilities TheCrossroadsDay where
  getAbilities (TheCrossroadsDay a) =
    extendRevealed
      a
      [ groupLimit PerGame $ restricted a 1 Here actionAbility
      , groupLimit PerRound
          $ restricted a 2 (Here <> oneOf (map PlayerCountIs [1, 2]))
          $ FastAbility' Free [#move]
      ]

instance RunMessage TheCrossroadsDay where
  runMessage msg l@(TheCrossroadsDay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid 10
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <- getAccessibleLocations iid (attrs.ability 2)
      chooseTargetM iid locations $ moveTo (attrs.ability 2) iid
      pure l
    _ -> TheCrossroadsDay <$> liftRunMessage msg attrs
