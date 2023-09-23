module Arkham.Event.Cards.BlindingLight2 (
  blindingLight2,
  blindingLight2Effect,
  BlindingLight2 (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Window qualified as Window

newtype BlindingLight2 = BlindingLight2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight2 :: EventCard BlindingLight2
blindingLight2 = event BlindingLight2 Cards.blindingLight2

instance RunMessage BlindingLight2 where
  runMessage msg e@(BlindingLight2 attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      pushAll
        [ createCardEffect Cards.blindingLight2 Nothing attrs iid
        , createCardEffect Cards.blindingLight2 Nothing attrs SkillTestTarget
        , chooseEvadeEnemy iid eid #willpower
        ]
      pure e
    _ -> BlindingLight2 <$> runMessage msg attrs

newtype BlindingLight2Effect = BlindingLight2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight2Effect :: EffectArgs -> BlindingLight2Effect
blindingLight2Effect = cardEffect BlindingLight2Effect Cards.blindingLight2

instance RunMessage BlindingLight2Effect where
  runMessage msg e@(BlindingLight2Effect attrs) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      when (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        $ pushAll
          [ If
              (Window.RevealChaosTokenEffect iid token (toId attrs))
              [LoseActions iid (toSource attrs) 1, assignHorror iid attrs 1]
          , disable attrs
          ]
      pure e
    PassedSkillTest iid (Just Action.Evade) _ (Initiator (EnemyTarget eid)) _ _ | SkillTestTarget == attrs.target -> do
      pushAll
        [ nonAttackEnemyDamage iid 2 eid
        , disable attrs
        ]
      pure e
    SkillTestEnds _ _ -> e <$ push (disable attrs)
    _ -> BlindingLight2Effect <$> runMessage msg attrs
