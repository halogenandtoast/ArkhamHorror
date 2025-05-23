module Arkham.Asset.Assets.ToolBelt (toolBelt) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement

newtype ToolBelt = ToolBelt AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toolBelt :: AssetCard ToolBelt
toolBelt = asset ToolBelt Cards.toolBelt

instance HasModifiersFor ToolBelt where
  getModifiersFor (ToolBelt a) = for_ a.controller \iid -> do
    modifySelect a (AssetAttachedToAsset (be a)) [Blank, AsIfUnderControlOf iid]

instance HasAbilities ToolBelt where
  getAbilities (ToolBelt a) = [controlled a 1 beltCriteria $ FastAbility (exhaust a)]
   where
    beltCriteria = any_ [AssetInPlayAreaOf You <> #tool, AssetAttachedToAsset (be a)]

instance RunMessage ToolBelt where
  runMessage msg a@(ToolBelt attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      inPlay <- select $ assetInPlayAreaOf iid <> #tool
      underneath <- selectWithField AssetCard $ AssetAttachedToAsset (be attrs)
      chooseOneM iid do
        labeled "Attach a Tool asset in your play area to Tool Belt" do
          when (notNull inPlay) do
            chooseTargetM iid inPlay (`place` AttachedToAsset attrs.id (Just $ InPlayArea iid))
        when (notNull inPlay && notNull underneath) do
          labeled "Switch a Tool asset in your play area with an attached asset" do
            chooseTargetM iid inPlay (`place` AttachedToAsset attrs.id (Just $ InPlayArea iid))
            focusCards (map snd underneath) do
              chooseOneM iid do
                for_ underneath \(x, _) -> targeting x $ place x (InPlayArea iid)
        when (notNull underneath) do
          labeled "Detach an attached asset." do
            focusCards (map snd underneath) do
              chooseOneM iid do
                for_ underneath \(x, _) -> targeting x $ place x (InPlayArea iid)
      pure a
    _ -> ToolBelt <$> liftRunMessage msg attrs
