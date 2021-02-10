module Arkham.Types.Asset.Cards.LeoDeLuca where

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

newtype LeoDeLuca = LeoDeLuca AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leoDeLuca :: AssetId -> LeoDeLuca
leoDeLuca uuid = LeoDeLuca $ (baseAttrs uuid "01048")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 2
  }

instance HasModifiersFor env LeoDeLuca where
  getModifiersFor _ (InvestigatorTarget iid) (LeoDeLuca a) =
    pure [ toModifier a (AdditionalActions 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env LeoDeLuca where
  getActions i window (LeoDeLuca x) = getActions i window x

instance (AssetRunner env) => RunMessage env LeoDeLuca where
  runMessage msg (LeoDeLuca attrs@AssetAttrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage $ GainActions iid (AssetSource aid) 1
      LeoDeLuca <$> runMessage msg attrs
    _ -> LeoDeLuca <$> runMessage msg attrs
