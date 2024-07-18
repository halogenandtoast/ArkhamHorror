module Arkham.Asset.Cards.Wither4 (wither4, wither4Effect, Wither4 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Fight
import Arkham.Matcher (InvestigatorMatcher (TurnInvestigator))
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype Wither4 = Wither4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wither4 :: AssetCard Wither4
wither4 = asset Wither4 Cards.wither4

instance HasAbilities Wither4 where
  getAbilities (Wither4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower (ActionCost 1)
    ]

instance RunMessage Wither4 where
  runMessage msg a@(Wither4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight iid source)
      pushAll
        $ [ createCardEffect Cards.wither4 Nothing source iid
          , skillTestModifier source iid (SkillModifier #willpower 2)
          ]
        <> chooseFight
      pure a
    _ -> Wither4 <$> runMessage msg attrs

newtype Wither4Effect = Wither4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wither4Effect :: EffectArgs -> Wither4Effect
wither4Effect = cardEffect Wither4Effect Cards.wither4

instance RunMessage Wither4Effect where
  runMessage msg e@(Wither4Effect attrs) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      enemyId <- fromJustNote "no attacked enemy" <$> getAttackedEnemy
      when (token.face `elem` [Skull, Cultist, Tablet, ElderThing]) do
        iid' <- selectJust TurnInvestigator
        pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token attrs.id)
              [ CreateWindowModifierEffect
                  (EffectTurnWindow iid')
                  ( EffectModifiers
                      $ toModifiers
                        attrs
                        [EnemyFightWithMin (-1) (Min 1), EnemyEvadeWithMin (-1) (Min 1), HealthModifierWithMin (-1) (Min 1)]
                  )
                  attrs.source
                  (toTarget enemyId)
              ]
          , disable attrs
          ]
      pure e
    SkillTestEnds _ _ -> e <$ push (disable attrs)
    _ -> Wither4Effect <$> runMessage msg attrs
