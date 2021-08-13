module Arkham.Types.Asset.Cards.DigDeep2
  ( DigDeep2(..)
  , digDeep2
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

newtype DigDeep2 = DigDeep2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

digDeep2 :: AssetCard DigDeep2
digDeep2 = asset DigDeep2 Cards.digDeep2

instance HasActions DigDeep2 where
  getActions (DigDeep2 a) =
    [ restrictedAbility a idx (OwnsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    | idx <- [1, 2]
    ]

instance (AssetRunner env) => RunMessage env DigDeep2 where
  runMessage msg a@(DigDeep2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillWillpower 1)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillAgility 1)
      )
    _ -> DigDeep2 <$> runMessage msg attrs
