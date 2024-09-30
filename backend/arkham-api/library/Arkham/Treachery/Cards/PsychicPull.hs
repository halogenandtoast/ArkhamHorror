module Arkham.Treachery.Cards.PsychicPull (psychicPull, PsychicPull (..)) where

import Arkham.Card.Cost
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
      hand <- iid.hand
      case nonEmpty hand of
        Nothing -> gainSurge attrs
        Just cards -> do
          c <- sample cards
          discardCard iid attrs c
          for_ c.cost \cost -> do
            sid <- getRandom
            revelationSkillTest sid iid attrs #willpower (Fixed $ toPrintedCost cost)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ LoseActions iid (toSource attrs) 1
      pure t
    _ -> PsychicPull <$> liftRunMessage msg attrs
