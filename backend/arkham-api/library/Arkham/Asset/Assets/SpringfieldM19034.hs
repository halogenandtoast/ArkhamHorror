module Arkham.Asset.Assets.SpringfieldM19034 (springfieldM19034) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Taboo

newtype SpringfieldM19034 = SpringfieldM19034 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

springfieldM19034 :: AssetCard SpringfieldM19034
springfieldM19034 = asset SpringfieldM19034 Cards.springfieldM19034

instance HasModifiersFor SpringfieldM19034 where
  getModifiersFor (SpringfieldM19034 a) =
    when (tabooed TabooList19 a) do
      for_ a.controller \iid -> do
        modified_
          a
          (AbilityTarget iid $ AbilityRef (toSource a) 1)
          [ canFightOverride
              $ EnemyWithoutModifier CannotBeAttacked
              <> not_ (enemyEngagedWith iid)
              <> oneOf
                [ at_ YourLocation
                , NonEliteEnemy <> at_ (ConnectedTo YourLocation)
                ]
          ]

-- TODO: Can't fight enemies engaged, see Telescopic Sight (3)
instance HasAbilities SpringfieldM19034 where
  getAbilities (SpringfieldM19034 a) =
    [ controlled a 1 (exists $ CanFightEnemy (a.ability 1) <> not_ EnemyEngagedWithYou)
        $ fightAction (assetUseCost a Ammo 1)
    ]

instance RunMessage SpringfieldM19034 where
  runMessage msg a@(SpringfieldM19034 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      let tabooExtend =
            if tabooed TabooList19 attrs
              then
                fightOverride
                  . ( <>
                        oneOf
                          [ EnemyAt $ locationWithInvestigator iid
                          , NonEliteEnemy <> EnemyAt (ConnectedTo $ locationWithInvestigator iid)
                          ]
                    )
              else id
      sid <- getRandom
      skillTestModifiers sid attrs iid $ DamageDealt 2
        : SkillModifier #combat 3
        : [IgnoreRetaliate | tabooed TabooList19 attrs]
      chooseFightEnemyMatch sid iid source (tabooExtend (not_ (enemyEngagedWith iid)))

      pure a
    _ -> SpringfieldM19034 <$> liftRunMessage msg attrs
