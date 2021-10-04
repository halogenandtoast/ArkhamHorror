module Arkham.Types.Asset.Cards.HardKnocks2
  ( HardKnocks2(..)
  , hardKnocks2
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype HardKnocks2 = HardKnocks2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardKnocks2 :: AssetCard HardKnocks2
hardKnocks2 = asset HardKnocks2 Cards.hardKnocks2

instance HasAbilities HardKnocks2 where
  getAbilities (HardKnocks2 a) =
    [ restrictedAbility a idx (OwnsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    | idx <- [1, 2]
    ]

instance AssetRunner env => RunMessage env HardKnocks2 where
  runMessage msg a@(HardKnocks2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillAgility 1)
      )
    _ -> HardKnocks2 <$> runMessage msg attrs
