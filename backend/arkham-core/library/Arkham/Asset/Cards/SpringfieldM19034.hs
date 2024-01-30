module Arkham.Asset.Cards.SpringfieldM19034 (springfieldM19034, SpringfieldM19034 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype SpringfieldM19034 = SpringfieldM19034 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

springfieldM19034 :: AssetCard SpringfieldM19034
springfieldM19034 = asset SpringfieldM19034 Cards.springfieldM19034

-- TODO: Can't fight enemies engaged, see Telescopic Sight (3)
instance HasAbilities SpringfieldM19034 where
  getAbilities (SpringfieldM19034 a) =
    [ controlledAbility a 1 (exists $ CanFightEnemy (a.ability 1) <> not_ EnemyEngagedWithYou)
        $ fightAction (assetUseCost a Ammo 1)
    ]

instance RunMessage SpringfieldM19034 where
  runMessage msg a@(SpringfieldM19034 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ skillTestModifiers attrs iid [DamageDealt 2, SkillModifier #combat 3]
        , ChooseFightEnemy iid (attrs.ability 1) Nothing #combat EnemyNotEngagedWithYou False
        ]
      pure a
    _ -> SpringfieldM19034 <$> runMessage msg attrs
