module Arkham.Types.Effect.Effects.NathanielCho
  ( NathanielCho(..)
  , nathanielCho
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype NathanielCho = NathanielCho EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nathanielCho :: EffectArgs -> NathanielCho
nathanielCho = NathanielCho . uncurry4 (baseAttrs "60101")

instance HasModifiersFor env NathanielCho

instance (HasList DiscardedPlayerCard env InvestigatorId, HasQueue env) => RunMessage env NathanielCho where
  runMessage msg e@(NathanielCho attrs) = case msg of
    PassedSkillTest iid _ _ _ _ _ -> do
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
    SkillTestEnds _ -> e <$ push (DisableEffect $ toId attrs)
    _ -> NathanielCho <$> runMessage msg attrs
