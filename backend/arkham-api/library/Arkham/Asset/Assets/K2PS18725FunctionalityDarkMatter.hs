module Arkham.Asset.Assets.K2PS18725FunctionalityDarkMatter (k2PS18725FunctionalityDarkMatter) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype K2PS18725FunctionalityDarkMatter = K2PS18725FunctionalityDarkMatter AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

k2PS18725FunctionalityDarkMatter :: AssetCard K2PS18725FunctionalityDarkMatter
k2PS18725FunctionalityDarkMatter = asset K2PS18725FunctionalityDarkMatter Cards.k2PS18725FunctionalityDarkMatter

instance HasAbilities K2PS18725FunctionalityDarkMatter where
  getAbilities (K2PS18725FunctionalityDarkMatter a) =
    [restricted a 1 ControlsThis $ freeReaction (RoundBegins #when)]

instance RunMessage K2PS18725FunctionalityDarkMatter where
  runMessage msg a@(K2PS18725FunctionalityDarkMatter attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select Anyone
      chooseOneM iid $ targets investigators \iid' ->
        turnModifier iid' (attrs.ability 1) iid'
          $ GiveAdditionalAction
          $ AdditionalAction "K2-PS187" (toSource attrs)
          $ ActionRestrictedAdditionalAction #scan
      pure a
    _ -> K2PS18725FunctionalityDarkMatter <$> liftRunMessage msg attrs
