module Arkham.Asset.Cards.ArcaneEnlightenment
  ( ArcaneEnlightenment(..)
  , arcaneEnlightenment
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Modifier
import Arkham.Slot
import Arkham.Target
import Arkham.Trait

newtype ArcaneEnlightenment = ArcaneEnlightenment AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

arcaneEnlightenment :: AssetCard ArcaneEnlightenment
arcaneEnlightenment = asset ArcaneEnlightenment Cards.arcaneEnlightenment

instance HasModifiersFor env ArcaneEnlightenment where
  getModifiersFor _ (InvestigatorTarget iid) (ArcaneEnlightenment attrs) =
    pure [ toModifier attrs (HandSize 1) | ownedBy attrs iid ]
  getModifiersFor _ _ _ = pure []

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome Nothing

instance (AssetRunner env) => RunMessage env ArcaneEnlightenment where
  runMessage msg (ArcaneEnlightenment attrs) = case msg of
    InvestigatorPlayedAsset iid aid _ _ | aid == assetId attrs -> do
      push (AddSlot iid HandSlot (slot attrs))
      ArcaneEnlightenment <$> runMessage msg attrs
    _ -> ArcaneEnlightenment <$> runMessage msg attrs
