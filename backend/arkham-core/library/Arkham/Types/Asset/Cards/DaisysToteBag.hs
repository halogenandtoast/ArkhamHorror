module Arkham.Types.Asset.Cards.DaisysToteBag
  ( DaisysToteBag(..)
  , daisysToteBag
  )
where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype DaisysToteBag = DaisysToteBag AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBag :: AssetId -> DaisysToteBag
daisysToteBag uuid = DaisysToteBag $ baseAttrs uuid "01008"

instance HasModifiersFor env DaisysToteBag where
  getModifiersFor = noModifiersFor

instance HasActions env DaisysToteBag where
  getActions i window (DaisysToteBag x) = getActions i window x

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome Nothing

instance AssetRunner env => RunMessage env DaisysToteBag where
  runMessage msg (DaisysToteBag attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessages $ replicate 2 (AddSlot iid HandSlot (slot attrs))
      DaisysToteBag <$> runMessage msg attrs
    _ -> DaisysToteBag <$> runMessage msg attrs
