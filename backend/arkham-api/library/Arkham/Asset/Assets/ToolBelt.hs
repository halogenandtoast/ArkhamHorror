module Arkham.Asset.Assets.ToolBelt (toolBelt, ToolBelt (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype ToolBelt = ToolBelt AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toolBelt :: AssetCard ToolBelt
toolBelt = asset ToolBelt Cards.toolBelt

instance HasModifiersFor ToolBelt where
  getModifiersFor (ToolBelt a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> modifySelect a (AssetAttachedToAsset (be a)) [Blank, AsIfUnderControlOf iid]

instance HasAbilities ToolBelt where
  getAbilities (ToolBelt a) = [controlledAbility a 1 beltCriteria $ FastAbility (exhaust a)]
   where
    beltCriteria = any_ [AssetInPlayAreaOf You <> #tool, AssetAttachedToAsset (be a)]

instance RunMessage ToolBelt where
  runMessage msg a@(ToolBelt attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      inPlay <- select $ AssetInPlayAreaOf (InvestigatorWithId iid) <> #tool
      underneath <- selectWithField AssetCard $ AssetAttachedToAsset (be attrs)
      chooseOneM iid do
        labeled "Attach a Tool asset in your play area to Tool Belt" do
          when (notNull inPlay) do
            chooseOne
              iid
              [targetLabel x [PlaceAsset x (AttachedToAsset attrs.id (Just $ InPlayArea iid))] | x <- inPlay]
        when (notNull inPlay && notNull underneath) do
          labeled "Switch a Tool asset in your play area with an attached asset" do
            chooseOne
              iid
              [targetLabel x [PlaceAsset x (AttachedToAsset attrs.id (Just $ InPlayArea iid))] | x <- inPlay]
            focusCards (map snd underneath) \unfocus -> do
              chooseOne iid [targetLabel x [unfocus, PlaceAsset x (InPlayArea iid)] | (x, _) <- underneath]
        when (notNull underneath) do
          labeled "Detach an attached asset." do
            focusCards (map snd underneath) \unfocus -> do
              chooseOne iid [targetLabel x [unfocus, PlaceAsset x (InPlayArea iid)] | (x, _) <- underneath]
      pure a
    _ -> ToolBelt <$> liftRunMessage msg attrs
