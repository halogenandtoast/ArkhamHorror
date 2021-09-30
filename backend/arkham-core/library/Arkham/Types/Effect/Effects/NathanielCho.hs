module Arkham.Types.Effect.Effects.NathanielCho
  ( NathanielCho(..)
  , nathanielCho
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype NathanielCho = NathanielCho EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nathanielCho :: EffectArgs -> NathanielCho
nathanielCho = NathanielCho . uncurry4 (baseAttrs "60101")

instance HasModifiersFor env NathanielCho where
  getModifiersFor _ target@(EnemyTarget _) (NathanielCho attrs)
    | effectTarget attrs == target = pure $ toModifiers attrs [DamageTaken 1]
  getModifiersFor _ _ _ = pure []

isTakeDamage :: EffectAttrs -> Window -> Bool
isTakeDamage attrs window = case effectTarget attrs of
  EnemyTarget eid -> go eid
  _ -> False
 where
  go eid = case windowType window of
    Window.TakeDamage _ _ (EnemyTarget eid') -> eid == eid'
    _ -> False

instance (HasList DiscardedPlayerCard env InvestigatorId, HasQueue env) => RunMessage env NathanielCho where
  runMessage msg e@(NathanielCho attrs) = case msg of
    PassedSkillTest iid _ _ _ _ _
      | effectTarget attrs == InvestigatorTarget iid -> do
        discardedCards <- map unDiscardedPlayerCard <$> getList iid
        let events = filter ((== EventType) . toCardType) discardedCards
        e <$ pushAll
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
    SkillTestEnds _ -> e <$ push (DisableEffect $ toId attrs)
    _ -> NathanielCho <$> runMessage msg attrs
