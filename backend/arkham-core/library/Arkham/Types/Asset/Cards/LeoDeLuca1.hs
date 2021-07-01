module Arkham.Types.Asset.Cards.LeoDeLuca1 where

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

newtype LeoDeLuca1 = LeoDeLuca1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leoDeLuca1 :: AssetCard LeoDeLuca1
leoDeLuca1 = ally LeoDeLuca1 Cards.leoDeLuca1 (2, 2)

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
