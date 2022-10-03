module Arkham.Asset.Cards.Shrivelling
  ( Shrivelling(..)
  , shrivelling
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype Shrivelling = Shrivelling AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrivelling :: AssetCard Shrivelling
shrivelling = asset Shrivelling Cards.shrivelling

instance HasAbilities Shrivelling where
  getAbilities (Shrivelling a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbilityWithSkill
        (Just Action.Fight)
        SkillWillpower
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage Shrivelling where
  runMessage msg a@(Shrivelling attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers attrs (InvestigatorTarget iid) [DamageDealt 1]
      , CreateEffect "01060" Nothing source (InvestigatorTarget iid)
      , ChooseFightEnemy iid source Nothing SkillWillpower mempty False
      ]
    _ -> Shrivelling <$> runMessage msg attrs
