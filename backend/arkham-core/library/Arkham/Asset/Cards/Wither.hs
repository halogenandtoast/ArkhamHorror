module Arkham.Asset.Cards.Wither (wither, witherEffect, Wither (..)) where

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

newtype Wither = Wither AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wither :: AssetCard Wither
wither = asset Wither Cards.wither

instance HasAbilities Wither where
  getAbilities (Wither a) =
    [restrictedAbility a 1 ControlsThis $ ActionAbilityWithSkill [#fight] #willpower (ActionCost 1)]

instance RunMessage Wither where
  runMessage msg a@(Wither attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pushAll
        $ createCardEffect Cards.wither (effectMetaTarget sid) source iid
        : chooseFight
      pure a
    _ -> Wither <$> runMessage msg attrs

newtype WitherEffect = WitherEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witherEffect :: EffectArgs -> WitherEffect
witherEffect = cardEffect WitherEffect Cards.wither

instance RunMessage WitherEffect where
  runMessage msg e@(WitherEffect attrs) = case msg of
    RevealChaosToken _ iid token | isTarget iid attrs.target -> do
      withSkillTest \sid -> do
        enemyId <- fromJustNote "no attacked enemy" <$> getAttackedEnemy
        let triggers =
              token.face
                `elem` [Skull, Cultist, Tablet, ElderThing]
                && maybe False (isTarget sid) attrs.metaTarget
        when triggers do
          iid' <- selectJust TurnInvestigator
          pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token attrs.id)
                [ CreateWindowModifierEffect
                    (EffectTurnWindow iid')
                    ( effectModifiers attrs [EnemyFightWithMin (-1) (Min 1), EnemyEvadeWithMin (-1) (Min 1)]
                    )
                    attrs.source
                    (toTarget enemyId)
                ]
            , disable attrs
            ]
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      e <$ push (disable attrs)
    _ -> WitherEffect <$> runMessage msg attrs
