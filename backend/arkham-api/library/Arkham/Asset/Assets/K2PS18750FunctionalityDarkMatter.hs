module Arkham.Asset.Assets.K2PS18750FunctionalityDarkMatter (k2PS18750FunctionalityDarkMatter) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype K2PS18750FunctionalityDarkMatter = K2PS18750FunctionalityDarkMatter AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

k2PS18750FunctionalityDarkMatter :: AssetCard K2PS18750FunctionalityDarkMatter
k2PS18750FunctionalityDarkMatter = asset K2PS18750FunctionalityDarkMatter Cards.k2PS18750FunctionalityDarkMatter

instance HasAbilities K2PS18750FunctionalityDarkMatter where
  getAbilities (K2PS18750FunctionalityDarkMatter a) =
    [ playerLimit PerRound
        $ controlled a 1 (DuringPhase #investigation) (FastAbility Free)
    ]

instance RunMessage K2PS18750FunctionalityDarkMatter where
  runMessage msg a@(K2PS18750FunctionalityDarkMatter attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select Anyone
      chooseOneM iid $ targets investigators \iid' ->
        turnModifier iid' (attrs.ability 1) iid'
          $ GiveAdditionalAction
          $ AdditionalAction "K2-PS187" (toSource attrs)
          $ ActionRestrictedAdditionalAction #scan
      pure a
    _ -> K2PS18750FunctionalityDarkMatter <$> liftRunMessage msg attrs
