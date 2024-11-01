module Arkham.Event.Events.Premonition (premonition, Premonition (..)) where

import Arkham.Ability
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Message (MessageType (CheckWindowMessage), pattern CancelNext)
import Arkham.Placement
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
  runMessage msg e@(Premonition attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      pushAll
        [ PlaceEvent eid (InPlayArea iid)
        , RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) RemoveChaosTokens
        ]
      pure e
    RequestedChaosTokens (isSource attrs -> True) _ ts -> do
      pushAll
        $ concatMap (\t -> [SealChaosToken t, SealedChaosToken t (toTarget attrs)]) ts
        <> [ResetChaosTokens (toSource attrs)]
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let ts = eventSealedChaosTokens attrs
      pushAll
        $ CancelNext (toSource attrs) CheckWindowMessage
        : map UnsealChaosToken ts
          <> map ObtainChaosToken ts
          <> [ ReplaceCurrentDraw (toSource attrs) iid
                $ Choose (toSource attrs) 1 ResolveChoice [Resolved ts] [] Nothing
             ]
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> Premonition <$> liftRunMessage msg attrs
