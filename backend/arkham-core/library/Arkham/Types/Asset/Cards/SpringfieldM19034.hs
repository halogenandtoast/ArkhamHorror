module Arkham.Types.Asset.Cards.SpringfieldM19034
  ( springfieldM19034
  , SpringfieldM19034(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype SpringfieldM19034 = SpringfieldM19034 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

springfieldM19034 :: AssetCard SpringfieldM19034
springfieldM19034 = asset SpringfieldM19034 Cards.springfieldM19034

instance HasAbilities SpringfieldM19034 where
  getAbilities (SpringfieldM19034 a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, UseCost (toId a) Ammo 1])
    ]

instance AssetRunner env => RunMessage env SpringfieldM19034 where
  runMessage msg a@(SpringfieldM19034 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers
        attrs
        (InvestigatorTarget iid)
        [DamageDealt 2, SkillModifier SkillCombat 3]
      , ChooseFightEnemy
        iid
        source
        Nothing
        SkillCombat
        EnemyNotEngagedWithYou
        False
      ]
    _ -> SpringfieldM19034 <$> runMessage msg attrs
