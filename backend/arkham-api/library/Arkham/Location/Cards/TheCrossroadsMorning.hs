module Arkham.Location.Cards.TheCrossroadsMorning (theCrossroadsMorning) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype TheCrossroadsMorning = TheCrossroadsMorning LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCrossroadsMorning :: LocationCard TheCrossroadsMorning
theCrossroadsMorning = symbolLabel $ location TheCrossroadsMorning Cards.theCrossroadsMorning 3 (Static 0)

instance HasAbilities TheCrossroadsMorning where
  getAbilities (TheCrossroadsMorning a) =
    extendRevealed
      a
      [ groupLimit PerGame $ restricted a 1 Here actionAbility
      , groupLimit PerRound
          $ restricted a 2 (Here <> oneOf (map PlayerCountIs [1, 2]) <> DuringTurn You)
          $ FastAbility' Free #move
      ]

instance RunMessage TheCrossroadsMorning where
  runMessage msg l@(TheCrossroadsMorning attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 10
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <- getAccessibleLocations iid (attrs.ability 2)
      chooseTargetM iid locations $ moveTo (attrs.ability 2) iid
      pure l
    _ -> TheCrossroadsMorning <$> liftRunMessage msg attrs
