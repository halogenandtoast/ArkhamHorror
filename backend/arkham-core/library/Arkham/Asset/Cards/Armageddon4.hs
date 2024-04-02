module Arkham.Asset.Cards.Armageddon4 (armageddon4, armageddon4Effect, Armageddon4 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Effect.Runner
import Arkham.Fight
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Prelude

newtype Armageddon4 = Armageddon4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armageddon4 :: AssetCard Armageddon4
armageddon4 = asset Armageddon4 Cards.armageddon4

instance HasAbilities Armageddon4 where
  getAbilities (Armageddon4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage Armageddon4 where
  runMessage msg a@(Armageddon4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      chooseFight <- aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight iid source)
      pushAll
        $ [ skillTestModifiers source iid [DamageDealt 1, SkillModifier #willpower 2]
          , createCardEffect Cards.armageddon4 Nothing source iid
          ]
        <> leftOr chooseFight
      pure a
    _ -> Armageddon4 <$> runMessage msg attrs

newtype Armageddon4Effect = Armageddon4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armageddon4Effect :: EffectArgs -> Armageddon4Effect
armageddon4Effect = cardEffect Armageddon4Effect Cards.armageddon4

instance RunMessage Armageddon4Effect where
  runMessage msg e@(Armageddon4Effect attrs) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      case attrs.source of
        AbilitySource (AssetSource assetId) 1 ->
          when (token.face == #curse) do
            enemies <- select $ EnemyAt (locationWithInvestigator iid) <> EnemyCanBeDamagedBySource attrs.source
            stillInPlay <- selectAny $ AssetWithId assetId
            player <- getPlayer iid
            pushAll
              $ [ chooseOrRunOne
                  player
                  $ [Label "Place 1 Charge on Armageddon4" [AddUses assetId Charge 1] | stillInPlay]
                  <> [ Label
                        "Deal 1 damage to an enemy at your location"
                        [ chooseOne
                            player
                            [targetLabel enemy [EnemyDamage enemy $ nonAttack attrs.source 1] | enemy <- enemies]
                        ]
                     ]
                | stillInPlay || notNull enemies
                ]
        _ -> error "wrong source"
      pure e
    SkillTestEnds _ _ -> do
      push (disable attrs)
      pure e
    _ -> Armageddon4Effect <$> runMessage msg attrs
