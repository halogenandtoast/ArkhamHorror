module Arkham.Types.Asset.Cards.LeoDeLuca where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target

newtype LeoDeLuca = LeoDeLuca AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leoDeLuca :: AssetCard LeoDeLuca
leoDeLuca = ally LeoDeLuca Cards.leoDeLuca (2, 2)

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
