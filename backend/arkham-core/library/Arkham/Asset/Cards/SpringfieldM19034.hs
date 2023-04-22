module Arkham.Asset.Cards.SpringfieldM19034
  ( springfieldM19034
  , SpringfieldM19034(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype SpringfieldM19034 = SpringfieldM19034 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

springfieldM19034 :: AssetCard SpringfieldM19034
springfieldM19034 = asset SpringfieldM19034 Cards.springfieldM19034

-- TODO: Can't fight enemies engaged, see Telescopic Sight (3)
instance HasAbilities SpringfieldM19034 where
  getAbilities (SpringfieldM19034 a) =
    [ restrictedAbility a 1 (ControlsThis <> EnemyCriteria (EnemyExists $ CanFightEnemy (toAbilitySource a 1) <> NotEnemy EnemyEngagedWithYou))
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage SpringfieldM19034 where
  runMessage msg a@(SpringfieldM19034 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ pushAll
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
