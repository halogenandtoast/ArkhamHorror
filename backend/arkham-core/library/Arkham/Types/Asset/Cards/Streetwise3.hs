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
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Streetwise3 = Streetwise3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetwise3 :: AssetCard Streetwise3
streetwise3 = asset Streetwise3 Cards.streetwise3

instance HasActions Streetwise3 where
  getActions (Streetwise3 a) =
    [ restrictedAbility
        a
        idx
        (OwnsThis <> DuringSkillTest AnySkillTest)
        (FastAbility $ ResourceCost 2)
    | idx <- [1, 2]
    ]

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
