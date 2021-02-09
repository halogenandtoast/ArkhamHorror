module Arkham.Types.Asset.Cards.Bandolier
  ( Bandolier(..)
  , bandolier
  )
where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype Bandolier = Bandolier AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bandolier :: AssetId -> Bandolier
bandolier uuid = Bandolier
  $ (baseAttrs uuid "02147") { assetHealth = Just 1, assetSlots = [BodySlot] }

instance HasModifiersFor env Bandolier where
  getModifiersFor = noModifiersFor

instance HasActions env Bandolier where
  getActions iid window (Bandolier x) = getActions iid window x

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Weapon Nothing

instance AssetRunner env => RunMessage env Bandolier where
  runMessage msg (Bandolier attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessage $ AddSlot iid HandSlot (slot attrs)
      Bandolier <$> runMessage msg attrs
    _ -> Bandolier <$> runMessage msg attrs
