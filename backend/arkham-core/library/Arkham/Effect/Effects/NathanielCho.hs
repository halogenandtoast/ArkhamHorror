module Arkham.Effect.Effects.NathanielCho
  ( NathanielCho(..)
  , nathanielCho
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype NathanielCho = NathanielCho EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nathanielCho :: EffectArgs -> NathanielCho
nathanielCho = NathanielCho . uncurry4 (baseAttrs "60101")

instance HasModifiersFor NathanielCho where
  getModifiersFor target@(EnemyTarget _) (NathanielCho attrs)
    | effectTarget attrs == target = pure $ toModifiers attrs [DamageTaken 1]
  getModifiersFor _ _ = pure []

isTakeDamage :: EffectAttrs -> Window -> Bool
isTakeDamage attrs window = case effectTarget attrs of
  EnemyTarget eid -> go eid
  _ -> False
 where
  go eid = case windowType window of
    Window.TakeDamage _ _ (EnemyTarget eid') ->
      eid == eid' && windowTiming window == Timing.After
    _ -> False

instance RunMessage NathanielCho where
  runMessage msg e@(NathanielCho attrs) = case msg of
    PassedSkillTest iid _ _ _ _ _
      | effectTarget attrs == InvestigatorTarget iid -> do
        discardedCards <- field InvestigatorDiscard iid
        let events = filter ((== EventType) . toCardType) discardedCards
        if null events
          then e <$ push (DisableEffect $ toId attrs)
          else e <$ pushAll
            [ chooseOne
              iid
              [ TargetLabel
                  (CardIdTarget $ toCardId event)
                  [ReturnToHand iid (CardIdTarget $ toCardId event)]
              | event <- events
              ]
            , DisableEffect $ toId attrs
            ]
    CheckWindow _ windows' | any (isTakeDamage attrs) windows' ->
      e <$ push (DisableEffect $ toId attrs)
    SkillTestEnds _ _ -> e <$ push (DisableEffect $ toId attrs)
    _ -> NathanielCho <$> runMessage msg attrs
