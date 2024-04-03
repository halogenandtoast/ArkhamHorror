module Arkham.Event.Cards.TheTruthBeckons (theTruthBeckons, TheTruthBeckons (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.Prelude

newtype TheTruthBeckons = TheTruthBeckons EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTruthBeckons :: EventCard TheTruthBeckons
theTruthBeckons = eventWith TheTruthBeckons Cards.theTruthBeckons (setMeta @Bool False)

instance RunMessage TheTruthBeckons where
  runMessage msg e@(TheTruthBeckons attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      targets <-
        select $ CanMoveCloserToLocation (toSource attrs) (InvestigatorWithId iid) UnrevealedLocation
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [targetLabel target [HandleTargetChoice iid (toSource attrs) (toTarget target)] | target <- targets]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      pushAll [toMessage $ (move attrs iid lid) {moveMeans = Towards}, DoStep 1 msg]
      pure e
    Msg.RevealLocation {} -> do
      pure $ TheTruthBeckons $ setMeta @Bool True attrs
    DoStep 1 msg'@(HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid)) -> do
      let revealedLocation = toResult @Bool attrs.meta
      canStillMove <-
        lid <=~> CanMoveCloserToLocation (toSource attrs) (InvestigatorWithId iid) (LocationWithId lid)
      notEngaged <- iid <!=~> InvestigatorEngagedWith AnyEnemy

      when (traceShowId canStillMove && traceShowId notEngaged && traceShowId (not revealedLocation)) do
        pushAll [toMessage $ (move attrs iid lid) {moveMeans = Towards}, DoStep 1 msg']

      pure e
    _ -> TheTruthBeckons <$> runMessage msg attrs
