module Arkham.Types.Asset.Cards.Shrivelling3
  ( Shrivelling3(..)
  , shrivelling3
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Shrivelling3 = Shrivelling3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

shrivelling3 :: AssetCard Shrivelling3
shrivelling3 = asset Shrivelling3 Cards.shrivelling3

instance HasAbilities Shrivelling3 where
  getAbilities (Shrivelling3 a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, UseCost (toId a) Charge 1])
    ]

instance AssetRunner env => RunMessage env Shrivelling3 where
  runMessage msg a@(Shrivelling3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers
        attrs
        (InvestigatorTarget iid)
        [SkillModifier SkillWillpower 2, DamageDealt 1]
      , CreateEffect "01060" Nothing source (InvestigatorTarget iid)
      -- reusing shrivelling(0)'s effect
      , ChooseFightEnemy iid source Nothing SkillWillpower mempty False
      ]
    _ -> Shrivelling3 <$> runMessage msg attrs
