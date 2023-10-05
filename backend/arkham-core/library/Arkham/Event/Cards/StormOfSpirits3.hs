module Arkham.Event.Cards.StormOfSpirits3 (
  stormOfSpirits3,
  stormOfSpirits3Effect,
  StormOfSpirits3 (..),
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
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (AttackDamageEffect, RevealChaosToken)
import Arkham.Window qualified as Window

newtype StormOfSpirits3 = StormOfSpirits3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits3 :: EventCard StormOfSpirits3
stormOfSpirits3 = event StormOfSpirits3 Cards.stormOfSpirits3

instance RunMessage StormOfSpirits3 where
  runMessage msg e@(StormOfSpirits3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifier attrs iid (SkillModifier #willpower 2)
        , createCardEffect Cards.stormOfSpirits3 Nothing attrs iid
        , chooseFightEnemyWithTarget iid attrs attrs #willpower
        ]
      pure e
    Successful (Action.Fight, EnemyTarget eid) iid _ target _ | isTarget attrs target -> do
      let
        toMsg eid' =
          if eid == eid'
            then EnemyDamage eid' $ delayDamage $ attack attrs 3
            else EnemyDamage eid' $ delayDamage $ isDirect $ attack attrs 3

      eids <- selectList $ enemyAtLocationWith iid
      pushAll $ map toMsg eids <> map (checkDefeated attrs) eids
      pure e
    _ -> StormOfSpirits3 <$> runMessage msg attrs

newtype StormOfSpirits3Effect = StormOfSpirits3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits3Effect :: EffectArgs -> StormOfSpirits3Effect
stormOfSpirits3Effect = cardEffect StormOfSpirits3Effect Cards.stormOfSpirits3

instance RunMessage StormOfSpirits3Effect where
  runMessage msg e@(StormOfSpirits3Effect attrs) = case msg of
    RevealChaosToken _ iid token | toTarget iid == effectTarget attrs -> do
      let triggers = chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
      when triggers $ do
        enemy <- fromJustNote "must be enemy" . join . fmap (preview _EnemyTarget) <$> getSkillTestTarget
        iids <- selectList $ InvestigatorAt $ locationWithEnemy enemy
        pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token (toId attrs))
              [assignDamage iid' (effectSource attrs) 2 | iid' <- iids]
          , DisableEffect $ toId attrs
          ]
      pure e
    SkillTestEnds _ _ -> do
      push (DisableEffect $ toId attrs)
      pure e
    _ -> StormOfSpirits3Effect <$> runMessage msg attrs
