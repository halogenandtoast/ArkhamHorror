module Arkham.Types.Asset.Cards.Hyperawareness2
  ( Hyperawareness2(..)
  , hyperawareness2
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype Hyperawareness2 = Hyperawareness2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperawareness2 :: AssetCard Hyperawareness2
hyperawareness2 = asset Hyperawareness2 Cards.hyperawareness2

instance HasModifiersFor env Hyperawareness2

ability :: Int -> AssetAttrs -> Ability
ability idx a = mkAbility (toSource a) idx (FastAbility $ ResourceCost 1)

instance HasActions env Hyperawareness2 where
  getActions iid (WhenSkillTest SkillIntellect) (Hyperawareness2 a) = do
    pure [ UseAbility iid (ability 1 a) | ownedBy a iid ]
  getActions iid (WhenSkillTest SkillAgility) (Hyperawareness2 a) = do
    pure [ UseAbility iid (ability 2 a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Hyperawareness2 where
  runMessage msg a@(Hyperawareness2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 1)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillAgility 1)
      )
    _ -> Hyperawareness2 <$> runMessage msg attrs
