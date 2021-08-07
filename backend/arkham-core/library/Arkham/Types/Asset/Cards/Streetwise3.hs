module Arkham.Types.Asset.Cards.Streetwise3
  ( streetwise3
  , Streetwise3(..)
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

newtype Streetwise3 = Streetwise3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetwise3 :: AssetCard Streetwise3
streetwise3 = asset Streetwise3 Cards.streetwise3

instance HasActions env Streetwise3 where
  getActions iid (WhenSkillTest SkillIntellect) (Streetwise3 a)
    | ownedBy a iid = pure [mkAbility a 1 $ FastAbility $ ResourceCost 2]
  getActions iid (WhenSkillTest SkillAgility) (Streetwise3 a) | ownedBy a iid =
    pure [mkAbility a 2 $ FastAbility $ ResourceCost 2]
  getActions _ _ _ = pure []

instance HasModifiersFor env Streetwise3

instance AssetRunner env => RunMessage env Streetwise3 where
  runMessage msg a@(Streetwise3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 3)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillIntellect 3)
      )
    _ -> Streetwise3 <$> runMessage msg attrs
