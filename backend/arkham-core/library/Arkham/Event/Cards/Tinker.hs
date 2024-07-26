module Arkham.Event.Cards.Tinker (tinker, Tinker (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.SlotType

newtype Meta = Meta {ignoredSlot :: Maybe SlotType}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Tinker = Tinker (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tinker :: EventCard Tinker
tinker = event (Tinker . (`with` Meta Nothing)) Cards.tinker

instance HasModifiersFor Tinker where
  getModifiersFor (AssetTarget aid) (Tinker (With a meta)) = maybeModified a do
    guard $ AssetTarget aid `elem` a.placement.attachedTo
    slot <- hoistMaybe $ ignoredSlot meta
    pure [TakeUpFewerSlots slot 1]
  getModifiersFor _ _ = pure []

instance RunMessage Tinker where
  runMessage msg e@(Tinker (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs
        $ assetInPlayAreaOf iid
        <> #tool
        <> not_ (AssetWithAttachedEvent $ eventIs Cards.tinker)
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      push $ PlaceEvent iid attrs.id $ AttachedToAsset aid Nothing
      slots <- field AssetSlots aid
      case (#hand `elem` slots, #accessory `elem` slots) of
        (True, False) -> doStep 1 msg
        (False, True) -> doStep 2 msg
        (False, False) -> pure ()
        (True, True) -> chooseOne iid [Label "Hand" [DoStep 1 msg], Label "Accessory" [DoStep 2 msg]]
      pure e
    DoStep 1 (HandleTargetChoice iid (isSource attrs -> True) _) -> do
      push $ InvestigatorClearUnusedAssetSlots iid
      pure . Tinker $ attrs `with` Meta {ignoredSlot = Just #hand}
    DoStep 2 (HandleTargetChoice iid (isSource attrs -> True) _) -> do
      push $ InvestigatorClearUnusedAssetSlots iid
      pure . Tinker $ attrs `with` Meta {ignoredSlot = Just #accessory}
    _ -> Tinker . (`with` meta) <$> liftRunMessage msg attrs
