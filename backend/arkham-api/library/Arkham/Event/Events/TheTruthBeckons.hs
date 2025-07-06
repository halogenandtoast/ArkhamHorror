module Arkham.Event.Events.TheTruthBeckons (theTruthBeckons) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Move

newtype TheTruthBeckons = TheTruthBeckons EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTruthBeckons :: EventCard TheTruthBeckons
theTruthBeckons = eventWith TheTruthBeckons Cards.theTruthBeckons (setMeta @Bool False)

instance RunMessage TheTruthBeckons where
  runMessage msg e@(TheTruthBeckons attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      locations <-
        select $ CanMoveCloserToLocation (toSource attrs) (InvestigatorWithId iid) UnrevealedLocation
      chooseTargetM iid locations $ handleTarget iid attrs
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      moveTowards attrs iid lid
      doStep 1 msg
      pure e
    Msg.RevealLocation {} -> pure $ TheTruthBeckons $ setMeta @Bool True attrs
    DoStep 1 msg'@(HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid)) -> do
      let revealedLocation = toResult @Bool attrs.meta
      canStillMove <-
        lid <=~> CanMoveCloserToLocation (toSource attrs) (InvestigatorWithId iid) (LocationWithId lid)
      notEngaged <- iid <!=~> InvestigatorEngagedWith AnyEnemy

      when (canStillMove && notEngaged && not revealedLocation) do
        moveTowards attrs iid lid
        doStep 1 msg'
      pure e
    _ -> TheTruthBeckons <$> liftRunMessage msg attrs
