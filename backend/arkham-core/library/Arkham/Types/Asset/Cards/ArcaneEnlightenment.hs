module Arkham.Types.Asset.Cards.ArcaneEnlightenment
  ( ArcaneEnlightenment(..)
  , arcaneEnlightenment
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Slot
import Arkham.Types.Target
import Arkham.Types.Trait

newtype ArcaneEnlightenment = ArcaneEnlightenment AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

arcaneEnlightenment :: AssetCard ArcaneEnlightenment
arcaneEnlightenment = arcane ArcaneEnlightenment Cards.arcaneEnlightenment

instance HasModifiersFor env ArcaneEnlightenment where
  getModifiersFor _ (InvestigatorTarget iid) (ArcaneEnlightenment attrs) =
    pure [ toModifier attrs (HandSize 1) | ownedBy attrs iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env ArcaneEnlightenment where
  getActions i window (ArcaneEnlightenment x) = getActions i window x

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome Nothing

instance (AssetRunner env) => RunMessage env ArcaneEnlightenment where
  runMessage msg (ArcaneEnlightenment attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessage (AddSlot iid HandSlot (slot attrs))
      ArcaneEnlightenment <$> runMessage msg attrs
    _ -> ArcaneEnlightenment <$> runMessage msg attrs
