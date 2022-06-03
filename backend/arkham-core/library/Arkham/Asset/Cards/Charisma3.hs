module Arkham.Asset.Cards.Charisma3
  ( charisma3
  , Charisma3(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Slot

newtype Charisma3 = Charisma3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charisma3 :: AssetCard Charisma3
charisma3 = asset Charisma3 Cards.charisma3

slot :: AssetAttrs -> Slot
slot attrs = Slot (toSource attrs) Nothing

instance AssetRunner env => RunMessage Charisma3 where
  runMessage msg (Charisma3 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      push $ AddSlot iid AllySlot (slot attrs)
      Charisma3 <$> runMessage msg attrs
    _ -> Charisma3 <$> runMessage msg attrs
