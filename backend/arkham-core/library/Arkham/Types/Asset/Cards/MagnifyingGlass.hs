module Arkham.Types.Asset.Cards.MagnifyingGlass where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype MagnifyingGlass = MagnifyingGlass AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

magnifyingGlass :: AssetCard MagnifyingGlass
magnifyingGlass = asset MagnifyingGlass Cards.magnifyingGlass

instance HasModifiersFor env MagnifyingGlass where
  getModifiersFor _ (InvestigatorTarget iid) (MagnifyingGlass a) = pure
    [ toModifier a $ ActionSkillModifier Action.Investigate SkillIntellect 1
    | ownedBy a iid
    ]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env MagnifyingGlass where
  runMessage msg (MagnifyingGlass attrs) =
    MagnifyingGlass <$> runMessage msg attrs
