module Arkham.Treachery.Cards.ConspiracyInRed (conspiracyInRed) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ConspiracyInRed = ConspiracyInRed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conspiracyInRed :: TreacheryCard ConspiracyInRed
conspiracyInRed = treachery ConspiracyInRed Cards.conspiracyInRed

instance RunMessage ConspiracyInRed where
  runMessage msg t@(ConspiracyInRed attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      inShadows <- select $ EnemyWithPlacement InTheShadows
      if null inShadows
        then findAndDrawEncounterCard iid $ #enemy <> CardWithConcealed
        else chooseTargetM iid inShadows $ placeDoomOn attrs 1
      pure t
    _ -> ConspiracyInRed <$> liftRunMessage msg attrs
