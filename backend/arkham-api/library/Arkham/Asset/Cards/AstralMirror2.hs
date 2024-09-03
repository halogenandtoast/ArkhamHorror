module Arkham.Asset.Cards.AstralMirror2 (astralMirror2, AstralMirror2 (..)) where

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Slot (isEmptySlot)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Slot

newtype Meta = Meta {emptyArcaneSlots :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype AstralMirror2 = AstralMirror2 (With AssetAttrs Meta)
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

astralMirror2 :: AssetCard AstralMirror2
astralMirror2 = asset (AstralMirror2 . (`with` Meta 0)) Cards.astralMirror2

instance HasModifiersFor AstralMirror2 where
  getModifiersFor (InvestigatorTarget iid) (AstralMirror2 (With attrs _)) | attrs `controlledBy` iid = do
    pure
      $ toModifiers
        attrs
        [ GiveAdditionalAction
            $ AdditionalAction "Astral Mirror (2)" (toSource attrs)
            $ PlayCardRestrictedAdditionalAction (basic #asset <> WillGoIntoSlot #hand)
        ]
  getModifiersFor _ _ = pure []

instance RunMessage AstralMirror2 where
  runMessage msg a@(AstralMirror2 (With attrs meta)) = runQueueT $ case msg of
    RemoveSlotFrom {} -> pure a -- prevent infinite recursion
    AddSlot {} -> pure a -- prevent infinite recursion
    _ -> do
      attrs' <- liftRunMessage msg attrs
      case attrs.owner of
        Nothing -> pure $ AstralMirror2 $ attrs' `with` meta
        Just iid -> do
          slots <- fieldMap InvestigatorSlots (count isEmptySlot . findWithDefault [] #arcane) iid
          let remove = emptyArcaneSlots meta - slots
          let add = slots - emptyArcaneSlots meta
          when (remove > 0) do
            replicateM_ remove $ push $ RemoveSlotFrom iid (toSource attrs) #arcane
          when (add > 0) do
            replicateM_ add $ push $ AddSlot iid #hand (Slot (toSource attrs) [])
          pure $ AstralMirror2 $ attrs' `with` Meta slots
