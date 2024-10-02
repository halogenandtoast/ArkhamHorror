module Arkham.Asset.Assets.Bandolier2 (Bandolier2 (..), bandolier2) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Slot
import Arkham.Investigator.Projection (getSlots)
import Arkham.Matcher
import Arkham.Trait

newtype Bandolier2 = Bandolier2 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bandolier2 :: AssetCard Bandolier2
bandolier2 = assetWith Bandolier2 Cards.bandolier2 (healthL ?~ 1)

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Weapon []

instance HasModifiersFor Bandolier2 where
  getModifiersFor (InvestigatorTarget iid) (Bandolier2 a) | controlledBy a iid = do
    n <- countM (anyM (<=~> asset_ #weapon) . slotItems) =<< getSlots #hand iid
    modified a [SkillModifier #willpower 1 | n >= 2]
  getModifiersFor _ _ = pure []

instance RunMessage Bandolier2 where
  runMessage msg (Bandolier2 attrs) = runQueueT $ case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardIsEnteringPlay iid card | card.id == attrs.cardId -> do
      pushAll [AddSlot iid #hand (slot attrs), AddSlot iid #hand (slot attrs)]
      Bandolier2 <$> liftRunMessage msg attrs
    _ -> Bandolier2 <$> liftRunMessage msg attrs
