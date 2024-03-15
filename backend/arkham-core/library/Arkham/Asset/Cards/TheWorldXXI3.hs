module Arkham.Asset.Cards.TheWorldXXI3 (
  theWorldXxi3,
  TheWorldXXI3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Window (defaultWindows)

newtype TheWorldXXI3 = TheWorldXXI3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWorldXxi3 :: AssetCard TheWorldXXI3
theWorldXxi3 = asset TheWorldXXI3 Cards.theWorldXxi3

instance HasAbilities TheWorldXXI3 where
  getAbilities (TheWorldXXI3 a) =
    [ fastAbility a 1 (exhaust a)
        $ ControlsThis
        <> InvestigatorExists (You <> HandWith (LengthIs $ atLeast 8))
    , restrictedAbility a 2 InYourHand $ freeReaction (GameBegins #when)
    ]

instance RunMessage TheWorldXXI3 where
  runMessage msg a@(TheWorldXXI3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    InHand _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> TheWorldXXI3 <$> runMessage msg attrs
