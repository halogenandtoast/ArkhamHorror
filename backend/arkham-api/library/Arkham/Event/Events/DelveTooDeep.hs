module Arkham.Event.Events.DelveTooDeep (delveTooDeep) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype DelveTooDeep = DelveTooDeep EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

delveTooDeep :: EventCard DelveTooDeep
delveTooDeep = event DelveTooDeep Cards.delveTooDeep

instance RunMessage DelveTooDeep where
  runMessage msg e@(DelveTooDeep attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      investigators <- select =<< guardAffectsOthers iid UneliminatedInvestigator
      for_ investigators \iid' ->
        chooseOneM iid' $ targeting EncounterDeckTarget $ drawEncounterCard iid' attrs
      push $ SetActiveInvestigator iid

      allInvestigators <- select UneliminatedInvestigator
      when (length investigators == length allInvestigators) $ addToVictory attrs
      pure e
    _ -> DelveTooDeep <$> liftRunMessage msg attrs
