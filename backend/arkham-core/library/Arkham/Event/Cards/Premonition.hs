module Arkham.Event.Cards.Premonition
  ( premonition
  , Premonition(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.RequestedTokenStrategy
import Arkham.Timing qualified as Timing

newtype Premonition = Premonition EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

premonition :: EventCard Premonition
premonition = event Premonition Cards.premonition

instance HasAbilities Premonition where
  getAbilities (Premonition a) =
    [ mkAbility a 1 $ ForcedAbility $ WouldRevealChaosToken Timing.When Anyone
    | notNull (eventSealedTokens a)
    ]

instance RunMessage Premonition where
  runMessage msg e@(Premonition attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ RequestTokens (toSource attrs) (Just iid) (Reveal 1) RemoveTokens
      pure e
    RequestedTokens (isSource attrs -> True) _ ts -> do
      pushAll $ concatMap (\t -> [SealToken t, SealedToken t (toCard attrs)]) ts <> [ResetTokens (toSource attrs)]
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let ts = eventSealedTokens attrs
      pushAll $
        CancelNext (toSource attrs) RunWindowMessage
        : map UnsealToken ts
        <> [ ReplaceCurrentDraw (toSource attrs) iid
               $ Choose 1 [Resolved ts] []
           ]
      pure e
    _ -> Premonition <$> runMessage msg attrs
