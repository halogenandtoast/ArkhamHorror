module Arkham.Asset.Cards.ToolBelt (toolBelt, ToolBelt (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection

newtype ToolBelt = ToolBelt AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toolBelt :: AssetCard ToolBelt
toolBelt = asset ToolBelt Cards.toolBelt

instance HasModifiersFor ToolBelt where
  getModifiersFor (AssetTarget aid) (ToolBelt a) | aid /= a.id = do
    maybeModified a do
      iid <- MaybeT $ field AssetController a.id
      AttachedToAsset aid' _ <- lift (field AssetPlacement aid)
      guard $ aid' == a.id
      pure [Blank, AsIfUnderControlOf iid]
  getModifiersFor _ _ = pure []

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
