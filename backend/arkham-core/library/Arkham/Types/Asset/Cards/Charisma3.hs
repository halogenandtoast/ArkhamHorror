module Arkham.Types.Asset.Cards.Charisma3
  ( charisma3
  , Charisma3(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot

newtype Charisma3 = Charisma3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charisma3 :: AssetCard Charisma3
charisma3 = asset Charisma3 Cards.charisma3

instance HasActions Charisma3
instance HasModifiersFor env Charisma3

slot :: AssetAttrs -> Slot
slot attrs = Slot (toSource attrs) Nothing

instance HasModifiersFor env () => RunMessage env Charisma3 where
  runMessage msg (Charisma3 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      push $ AddSlot iid AllySlot (slot attrs)
      Charisma3 <$> runMessage msg attrs
    _ -> Charisma3 <$> runMessage msg attrs
