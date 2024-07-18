module Arkham.Event.Cards.Premonition (premonition, Premonition (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy

newtype Premonition = Premonition EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

premonition :: EventCard Premonition
premonition = event Premonition Cards.premonition

instance HasAbilities Premonition where
  getAbilities (Premonition a) =
    [ mkAbility a 1 $ forced $ WouldRevealChaosToken #when Anyone
    | notNull (eventSealedChaosTokens a)
    ]

instance RunMessage Premonition where
  runMessage msg e@(Premonition attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      pushAll
        [ PlaceEvent iid eid (InPlayArea iid)
        , RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) RemoveChaosTokens
        ]
      pure e
    RequestedChaosTokens (isSource attrs -> True) _ ts -> do
      pushAll
        $ concatMap (\t -> [SealChaosToken t, SealedChaosToken t (toCard attrs)]) ts
        <> [ResetChaosTokens (toSource attrs)]
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let ts = eventSealedChaosTokens attrs
      pushAll
        $ CancelNext (toSource attrs) RunWindowMessage
        : map UnsealChaosToken ts
          <> [ ReplaceCurrentDraw (toSource attrs) iid
                $ Choose (toSource attrs) 1 ResolveChoice [Resolved ts] []
             ]
          <> [toDiscardBy iid (attrs.ability 1) attrs]
      pure e
    _ -> Premonition <$> runMessage msg attrs
