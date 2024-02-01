module Arkham.Asset.Cards.Bandolier2 (
  Bandolier2 (..),
  bandolier2,
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.SkillType
import Arkham.Trait

newtype Bandolier2 = Bandolier2 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

bandolier2 :: AssetCard Bandolier2
bandolier2 = assetWith Bandolier2 Cards.bandolier2 (healthL ?~ 1)

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Weapon []

instance HasModifiersFor Bandolier2 where
  getModifiersFor (InvestigatorTarget iid) (Bandolier2 a) | controlledBy a iid = do
    n <-
      fieldMap InvestigatorSlots (length . filter (not . isEmptySlot) . findWithDefault [] HandSlot) iid
    pure $ toModifiers a [SkillModifier SkillWillpower 1 | n >= 2]
  getModifiersFor _ _ = pure []

instance RunMessage Bandolier2 where
  runMessage msg (Bandolier2 attrs) = case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      pushAll [AddSlot iid HandSlot (slot attrs), AddSlot iid HandSlot (slot attrs)]
      Bandolier2 <$> runMessage msg attrs
    _ -> Bandolier2 <$> runMessage msg attrs
