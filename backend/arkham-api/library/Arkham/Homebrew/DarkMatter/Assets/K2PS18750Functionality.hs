module Arkham.Homebrew.DarkMatter.Assets.K2PS18750Functionality (k2PS18750Functionality) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Homebrew.DarkMatter.Actions (pattern Scan)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype K2PS18750Functionality = K2PS18750Functionality AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

k2PS18750Functionality :: AssetCard K2PS18750Functionality
k2PS18750Functionality = asset K2PS18750Functionality Cards.k2PS18750Functionality

instance HasAbilities K2PS18750Functionality where
  getAbilities (K2PS18750Functionality a) =
    [ playerLimit PerRound
        $ controlled a 1 (DuringPhase #investigation) (FastAbility Free)
    ]

instance RunMessage K2PS18750Functionality where
  runMessage msg a@(K2PS18750Functionality attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select Anyone
      chooseOneM iid $ targets investigators \iid' ->
        turnModifier iid' (attrs.ability 1) iid'
          $ GiveAdditionalAction
          $ AdditionalAction "K2-PS187" (toSource attrs)
          $ ActionRestrictedAdditionalAction Scan
      pure a
    _ -> K2PS18750Functionality <$> liftRunMessage msg attrs
