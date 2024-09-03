module Arkham.Asset.Cards.SpringfieldM19034 (springfieldM19034, SpringfieldM19034 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Taboo

newtype SpringfieldM19034 = SpringfieldM19034 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

springfieldM19034 :: AssetCard SpringfieldM19034
springfieldM19034 = asset SpringfieldM19034 Cards.springfieldM19034

instance HasModifiersFor SpringfieldM19034 where
  getModifiersFor (AbilityTarget iid ability) (SpringfieldM19034 a)
    | tabooed TabooList19 a && isSource a ability.source && ability.index == 1 =
        pure
          $ toModifiers
            a
            [ CanModify
                $ EnemyFightActionCriteria
                $ CriteriaOverride
                $ EnemyCriteria
                $ ThisEnemy
                $ EnemyWithoutModifier CannotBeAttacked
                <> not_ (enemyEngagedWith iid)
                <> oneOf
                  [ EnemyAt YourLocation
                  , NonEliteEnemy <> EnemyAt (ConnectedTo YourLocation)
                  ]
            ]
  getModifiersFor _ _ = pure []

-- TODO: Can't fight enemies engaged, see Telescopic Sight (3)
instance HasAbilities SpringfieldM19034 where
  getAbilities (SpringfieldM19034 a) =
    [ controlledAbility
        a
        1
        (exists $ CanFightEnemy (a.ability 1) <> not_ EnemyEngagedWithYou)
        $ fightAction (assetUseCost a Ammo 1)
    ]

instance RunMessage SpringfieldM19034 where
  runMessage msg a@(SpringfieldM19034 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      let tabooExtend =
            if tabooed TabooList19 attrs
              then
                ( <>
                    oneOf
                      [ EnemyAt $ locationWithInvestigator iid
                      , NonEliteEnemy <> EnemyAt (ConnectedFrom $ locationWithInvestigator iid)
                      ]
                )
              else id
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFightMatch sid iid source (tabooExtend EnemyNotEngagedWithYou)
      pushAll
        [ skillTestModifiers sid attrs iid $ DamageDealt 2
            : SkillModifier #combat 3
            : [IgnoreRetaliate | tabooed TabooList19 attrs]
        , chooseFight
        ]
      pure a
    _ -> SpringfieldM19034 <$> runMessage msg attrs
