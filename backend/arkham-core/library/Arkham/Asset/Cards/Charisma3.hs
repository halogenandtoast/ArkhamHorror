module Arkham.Asset.Cards.Charisma3 (
  charisma3,
  Charisma3 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card

newtype Charisma3 = Charisma3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

charisma3 :: AssetCard Charisma3
charisma3 = asset Charisma3 Cards.charisma3

slot :: AssetAttrs -> Slot
slot attrs = Slot (toSource attrs) []

instance RunMessage Charisma3 where
  runMessage msg (Charisma3 attrs) = case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid AllySlot (slot attrs)
      Charisma3 <$> runMessage msg attrs
    _ -> Charisma3 <$> runMessage msg attrs
