module Arkham.Asset.Assets.ToolBelt (toolBelt) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement

newtype ToolBelt = ToolBelt AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toolBelt :: AssetCard ToolBelt
toolBelt = asset ToolBelt Cards.toolBelt

instance HasModifiersFor ToolBelt where
  getModifiersFor (ToolBelt a) =
    modifySelect a (AssetAttachedToAsset (be a)) [Blank, DoNotTakeUpSlots]

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
        (cardI18n $ labeled' "toolBelt.attachAToolAssetInYourPlayAreaToToolBelt") do
          when (notNull inPlay) do
            chooseTargetM iid inPlay (`place` AttachedToAsset attrs.id (Just $ InPlayArea iid))
        when (notNull inPlay && notNull underneath) do
          (cardI18n $ labeled' "toolBelt.switchAToolAssetInYourPlayAreaWithAnAttachedAsset") do
            chooseTargetM iid inPlay (`place` AttachedToAsset attrs.id (Just $ InPlayArea iid))
            focusCards (map snd underneath) do
              chooseOneM iid do
                for_ underneath \(x, _) -> targeting x $ place x (InPlayArea iid)
        when (notNull underneath) do
          (cardI18n $ labeled' "toolBelt.detachAnAttachedAsset") do
            focusCards (map snd underneath) do
              chooseOneM iid do
                for_ underneath \(x, _) -> targeting x $ place x (InPlayArea iid)
      pure a
    _ -> ToolBelt <$> liftRunMessage msg attrs
