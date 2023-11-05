module Arkham.Asset.Cards.Safeguard2 (
  safeguard2,
  Safeguard2 (..),
)
where

import Arkham.Prelude

import Arkham.Ability hiding (DuringTurn)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype Safeguard2 = Safeguard2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

safeguard2 :: AssetCard Safeguard2
safeguard2 = asset Safeguard2 Cards.safeguard2

instance HasAbilities Safeguard2 where
  getAbilities (Safeguard2 a) =
    [restrictedAbility a 1 ControlsThis $ ReactionAbility (DuringTurn NotYou) (exhaust a)]

instance RunMessage Safeguard2 where
  runMessage msg a@(Safeguard2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iid' <- selectJust TurnInvestigator
      push $ turnModifier attrs iid $ CanMoveWith $ InvestigatorWithId iid'
      pure a
    _ -> Safeguard2 <$> runMessage msg attrs
