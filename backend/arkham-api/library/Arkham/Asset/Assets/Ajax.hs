module Arkham.Asset.Assets.Ajax (ajax) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.Defs (allActions)
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Trait (Trait (Field))

newtype Ajax = Ajax AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ajax :: AssetCard Ajax
ajax = assetWith Ajax Cards.ajax (healthL ?~ 2)

instance HasModifiersFor Ajax where
  getModifiersFor (Ajax a) = for_ a.controller \iid -> do
    actions <- fieldMap InvestigatorActionsPerformed concat iid
    when (null actions) do
      modified_ a iid $ ActionDoesNotCauseAttacksOfOpportunity <$> allActions

instance HasAbilities Ajax where
  getAbilities (Ajax a) =
    [ controlled a 1 (youExist InvestigatorCanMove)
        $ ActionAbility #move Nothing (ActionCost 1 <> exhaust a)
    , restricted a 2 (OnSameLocation <> youExist (not_ $ ControlsAsset $ AssetWithId a.id)) #action
    ]

instance RunMessage Ajax where
  runMessage msg a@(Ajax attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      fieldLocations <-
        select $ withTrait Field <> canEnterLocation iid <> not_ (locationWithInvestigator iid)
      chooseOneM iid $ cardI18n $ scope "ajax" do
        labeled' "moveTwice" $ doStep 2 msg
        when (notNull fieldLocations) do
          labeled' "moveToField" do
            chooseTargetM iid fieldLocations $ moveTo (attrs.ability 1) iid
      pure a
    DoStep 2 msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations \loc -> do
        moveTo (attrs.ability 1) iid loc
        doStep 1 msg'
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      takeControlOfAsset iid attrs
      pure a
    _ -> Ajax <$> liftRunMessage msg attrs
