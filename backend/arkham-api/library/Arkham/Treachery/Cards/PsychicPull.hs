module Arkham.Treachery.Cards.PsychicPull (psychicPull) where

import Arkham.Card.Cost
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Investigator.Projection ()
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PsychicPull = PsychicPull TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychicPull :: TreacheryCard PsychicPull
psychicPull = treachery PsychicPull Cards.psychicPull

instance RunMessage PsychicPull where
  runMessage msg t@(PsychicPull attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      randomDiscardEdit iid attrs \d -> d {discardTarget = Just (toTarget attrs)}
      pure t
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      let
        totalPrintedCost =
          case mapMaybe (.cost) cards of
            [] -> 0
            costs -> sum (map toPrintedCost costs)
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed totalPrintedCost)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      loseActions iid attrs 1
      pure t
    _ -> PsychicPull <$> liftRunMessage msg attrs
