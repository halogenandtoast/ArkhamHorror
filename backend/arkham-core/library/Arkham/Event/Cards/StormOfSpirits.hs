module Arkham.Event.Cards.StormOfSpirits (
  stormOfSpirits,
  stormOfSpiritsEffect,
  StormOfSpirits (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher hiding (AttackDamageEffect, RevealChaosToken)
import Arkham.Window qualified as Window

newtype StormOfSpirits = StormOfSpirits EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits :: EventCard StormOfSpirits
stormOfSpirits = event StormOfSpirits Cards.stormOfSpirits

instance RunMessage StormOfSpirits where
  runMessage msg e@(StormOfSpirits attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ createCardEffect Cards.stormOfSpirits Nothing attrs iid
        , chooseFightEnemyWithTarget iid attrs attrs #willpower
        ]
      pure e
    Successful (Action.Fight, EnemyTarget eid) iid _ target _ | isTarget attrs target -> do
      let
        toMsg eid' =
          if eid == eid'
            then EnemyDamage eid' $ delayDamage $ attack attrs 2
            else EnemyDamage eid' $ delayDamage $ isDirect $ attack attrs 2
      msgs <- selectListMap toMsg $ enemyAtLocationWith iid
      pushAll $ msgs <> [CheckDefeated (toSource attrs)]
      pure e
    _ -> StormOfSpirits <$> runMessage msg attrs

newtype StormOfSpiritsEffect = StormOfSpiritsEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpiritsEffect :: EffectArgs -> StormOfSpiritsEffect
stormOfSpiritsEffect = cardEffect StormOfSpiritsEffect Cards.stormOfSpirits

instance RunMessage StormOfSpiritsEffect where
  runMessage msg e@(StormOfSpiritsEffect attrs) = case msg of
    RevealChaosToken _ iid token | toTarget iid == effectTarget attrs -> do
      let triggers = chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
      when triggers $ do
        enemy <- fromJustNote "must be enemy" . join . fmap (preview _EnemyTarget) <$> getSkillTestTarget
        iids <- selectList $ InvestigatorAt $ locationWithEnemy enemy
        pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token (toId attrs))
              [assignDamage iid' (effectSource attrs) 1 | iid' <- iids]
          , DisableEffect $ toId attrs
          ]
      pure e
    SkillTestEnds _ _ -> do
      push (DisableEffect $ toId attrs)
      pure e
    _ -> StormOfSpiritsEffect <$> runMessage msg attrs
