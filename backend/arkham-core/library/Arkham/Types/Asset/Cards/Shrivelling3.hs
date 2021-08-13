module Arkham.Types.Asset.Cards.Shrivelling3
  ( Shrivelling3(..)
  , shrivelling3
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Shrivelling3 = Shrivelling3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

shrivelling3 :: AssetCard Shrivelling3
shrivelling3 = arcane Shrivelling3 Cards.shrivelling3

instance HasActions Shrivelling3 where
  getActions (Shrivelling3 a) =
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
      , ChooseFightEnemy iid source SkillWillpower mempty False
      ]
    _ -> Shrivelling3 <$> runMessage msg attrs
