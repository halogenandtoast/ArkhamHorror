module Arkham.Types.Asset.Cards.LeoDeLuca1 where

import Arkham.Prelude

import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype LeoDeLuca1 = LeoDeLuca1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leoDeLuca1 :: AssetId -> LeoDeLuca1
leoDeLuca1 uuid = LeoDeLuca1 $ (baseAttrs uuid "01054")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 2
  }

instance HasModifiersFor env LeoDeLuca1 where
  getModifiersFor _ (InvestigatorTarget iid) (LeoDeLuca1 a) =
    pure [ toModifier a (AdditionalActions 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env LeoDeLuca1 where
  getActions i window (LeoDeLuca1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env LeoDeLuca1 where
  runMessage msg (LeoDeLuca1 attrs@AssetAttrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage $ GainActions iid (AssetSource aid) 1
      LeoDeLuca1 <$> runMessage msg attrs
    _ -> LeoDeLuca1 <$> runMessage msg attrs
