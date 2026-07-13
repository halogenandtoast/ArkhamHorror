module Arkham.Homebrew.DarkMatter.Assets.K2PS18725Functionality (k2PS18725Functionality) where

import Arkham.Ability
import Arkham.Action.Additional
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype K2PS18725Functionality = K2PS18725Functionality AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

k2PS18725Functionality :: AssetCard K2PS18725Functionality
k2PS18725Functionality = asset K2PS18725Functionality Cards.k2PS18725Functionality

instance HasAbilities K2PS18725Functionality where
  getAbilities (K2PS18725Functionality a) =
    [restricted a 1 ControlsThis $ freeReaction (RoundBegins #when)]

instance RunMessage K2PS18725Functionality where
  runMessage msg a@(K2PS18725Functionality attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select Anyone
      chooseOneM iid $ targets investigators \iid' ->
        turnModifier iid' (attrs.ability 1) iid'
          $ GiveAdditionalAction
          $ AdditionalAction "K2-PS187" (toSource attrs)
          $ ActionRestrictedAdditionalAction #scan
      pure a
    _ -> K2PS18725Functionality <$> liftRunMessage msg attrs
