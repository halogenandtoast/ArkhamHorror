module Arkham.Types.Asset.Cards.HardKnocks
  ( HardKnocks(..)
  , hardKnocks
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

newtype HardKnocks = HardKnocks AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardKnocks :: AssetCard HardKnocks
hardKnocks = asset HardKnocks Cards.hardKnocks

instance HasActions HardKnocks where
  getActions (HardKnocks a) =
    [ restrictedAbility a idx (OwnsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    | idx <- [1, 2]
    ]

instance (AssetRunner env) => RunMessage env HardKnocks where
  runMessage msg a@(HardKnocks attrs) = case msg of
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
    _ -> HardKnocks <$> runMessage msg attrs
