module Arkham.Types.Asset.Cards.Bandolier
  ( Bandolier(..)
  , bandolier
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Trait

newtype Bandolier = Bandolier AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bandolier :: AssetCard Bandolier
bandolier = bodyWith Bandolier Cards.bandolier (healthL ?~ 1)

instance HasModifiersFor env Bandolier

instance HasAbilities Bandolier

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Weapon Nothing

instance AssetRunner env => RunMessage env Bandolier where
  runMessage msg (Bandolier attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      push $ AddSlot iid HandSlot (slot attrs)
      Bandolier <$> runMessage msg attrs
    _ -> Bandolier <$> runMessage msg attrs
