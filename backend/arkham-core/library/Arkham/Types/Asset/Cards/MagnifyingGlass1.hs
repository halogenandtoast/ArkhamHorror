{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.MagnifyingGlass1 where

import Arkham.Json
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude

newtype MagnifyingGlass1 = MagnifyingGlass1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

magnifyingGlass1 :: AssetId -> MagnifyingGlass1
magnifyingGlass1 uuid =
  MagnifyingGlass1 $ (baseAttrs uuid "01040") { assetSlots = [HandSlot] }

instance (ActionRunner env investigator) => HasActions env investigator MagnifyingGlass1 where
  getActions i _ (MagnifyingGlass1 Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      clueCount <- unClueCount <$> asks (getCount (locationOf i))
      pure
        [ UseCardAbility
            (getId () i)
            (AssetSource assetId)
            (AssetSource assetId)
            Nothing
            1
        | clueCount == 0
        ]
  getActions i window (MagnifyingGlass1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env MagnifyingGlass1 where
  runMessage msg (MagnifyingGlass1 attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage
        (AddModifiers
          (InvestigatorTarget iid)
          (AssetSource aid)
          [ActionSkillModifier Action.Investigate SkillIntellect 1]
        )
      MagnifyingGlass1 <$> runMessage msg attrs
    _ -> MagnifyingGlass1 <$> runMessage msg attrs
