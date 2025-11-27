module Arkham.Treachery.Cards.BeyondThePale (beyondThePale) where

import Arkham.Matcher
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Trait (Trait (Hex))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BeyondThePale = BeyondThePale TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondThePale :: TreacheryCard BeyondThePale
beyondThePale = treachery BeyondThePale Cards.beyondThePale

instance RunMessage BeyondThePale where
  runMessage msg t@(BeyondThePale attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      doStep 1 msg
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      n <-
        selectCount $ TreacheryAttachedToLocation (locationWithInvestigator iid) <> TreacheryWithTrait Hex
      assignHorror iid attrs n
      pure t
    DoStep 1 (Revelation _ (isSource attrs -> True)) -> do
      xs <- select $ treacheryIs Cards.beyondThePale
      when (length xs == 3) $ for_ xs $ toDiscard attrs
      pure t
    _ -> BeyondThePale <$> liftRunMessage msg attrs
