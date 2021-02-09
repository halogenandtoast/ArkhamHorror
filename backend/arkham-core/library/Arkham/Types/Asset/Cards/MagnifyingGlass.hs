module Arkham.Types.Asset.Cards.MagnifyingGlass where


import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype MagnifyingGlass = MagnifyingGlass AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

magnifyingGlass :: AssetId -> MagnifyingGlass
magnifyingGlass uuid =
  MagnifyingGlass $ (baseAttrs uuid "01030") { assetSlots = [HandSlot] }

instance HasModifiersFor env MagnifyingGlass where
  getModifiersFor _ (InvestigatorTarget iid) (MagnifyingGlass a) = pure
    [ toModifier a $ ActionSkillModifier Action.Investigate SkillIntellect 1
    | ownedBy a iid
    ]
  getModifiersFor _ _ _ = pure []

instance HasActions env MagnifyingGlass where
  getActions i window (MagnifyingGlass x) = getActions i window x

instance (AssetRunner env) => RunMessage env MagnifyingGlass where
  runMessage msg (MagnifyingGlass attrs) =
    MagnifyingGlass <$> runMessage msg attrs
