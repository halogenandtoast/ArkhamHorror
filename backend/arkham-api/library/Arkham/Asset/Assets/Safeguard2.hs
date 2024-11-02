module Arkham.Asset.Assets.Safeguard2 (safeguard2, Safeguard2 (..)) where

import Arkham.Ability hiding (DuringTurn)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype Safeguard2 = Safeguard2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

safeguard2 :: AssetCard Safeguard2
safeguard2 = asset Safeguard2 Cards.safeguard2

instance HasAbilities Safeguard2 where
  getAbilities (Safeguard2 a) =
    [noLimit $ restricted a 1 ControlsThis $ ReactionAbility (DuringTurn NotYou) (exhaust a)]

instance RunMessage Safeguard2 where
  runMessage msg a@(Safeguard2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      iid' <- selectJust TurnInvestigator
      turnModifier iid' attrs iid $ CanMoveWith $ InvestigatorWithId iid'
      pure a
    _ -> Safeguard2 <$> liftRunMessage msg attrs
