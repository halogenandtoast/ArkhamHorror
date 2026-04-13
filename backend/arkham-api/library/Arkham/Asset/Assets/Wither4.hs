module Arkham.Asset.Assets.Wither4 (wither4, wither4Effect) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Effect.Window
import Arkham.Fight
import Arkham.Helpers.Modifiers (effectModifiers)
import Arkham.Helpers.SkillTest (getAttackedEnemy, withSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher (InvestigatorMatcher (TurnInvestigator))
import Arkham.Modifier
import Arkham.Projection
import Arkham.Taboo
import Arkham.Window qualified as Window

newtype Wither4 = Wither4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wither4 :: AssetCard Wither4
wither4 = asset Wither4 Cards.wither4

instance HasAbilities Wither4 where
  getAbilities (Wither4 a) = [controlled_ a 1 $ ActionAbility #fight #willpower (ActionCost 1)]

instance RunMessage Wither4 where
  runMessage msg a@(Wither4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      createCardEffect Cards.wither4 (effectMetaTarget sid) source iid
      skillTestModifier sid source iid (SkillModifier #willpower 2)
      aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    _ -> Wither4 <$> liftRunMessage msg attrs

newtype Wither4Effect = Wither4Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wither4Effect :: EffectArgs -> Wither4Effect
wither4Effect = cardEffect Wither4Effect Cards.wither4

instance RunMessage Wither4Effect where
  runMessage msg e@(Wither4Effect attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token | isTarget iid attrs.target -> do
      withSkillTest \sid -> do
        enemyId <- fromJustNote "no attacked enemy" <$> getAttackedEnemy
        isTaboo <- maybe False (>= TabooList25) <$> field InvestigatorTaboo iid
        let checkToken =
              if isTaboo
                then isSymbolChaosToken
                else (`elem` [Skull, Cultist, Tablet, ElderThing])
        let triggers =
              checkToken token.face
                && maybe False (isTarget sid) attrs.metaTarget
        when triggers do
          iid' <- selectJust TurnInvestigator
          ems <-
            effectModifiers
              attrs
              [EnemyFightWithMin (-1) (Min 1), EnemyEvadeWithMin (-1) (Min 1), HealthModifierWithMin (-1) (Min 1)]
          push
            $ If
              (Window.RevealChaosTokenEffect iid token attrs.id)
              [ CreateWindowModifierEffect
                  (if isTaboo then EffectPhaseWindow else EffectTurnWindow iid')
                  ems
                  attrs.source
                  (toTarget enemyId)
              ]
          disable attrs
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      disableReturn e
    _ -> Wither4Effect <$> liftRunMessage msg attrs
