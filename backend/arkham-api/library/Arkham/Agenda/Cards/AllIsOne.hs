module Arkham.Agenda.Cards.AllIsOne (allIsOne) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Log (whenHasRecord)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype AllIsOne = AllIsOne AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIsOne :: AgendaCard AllIsOne
allIsOne = agenda (1, A) AllIsOne Cards.allIsOne (Static 4)

instance HasAbilities AllIsOne where
  getAbilities (AllIsOne x) =
    [mkAbility x 1 $ forced $ MovedBy #after You Matcher.EncounterCardSource]

instance RunMessage AllIsOne where
  runMessage msg a@(AllIsOne attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      lead <- getLead
      discardUntilFirst lead attrs Deck.EncounterDeck (basic #location)
      whenHasRecord TheInvestigatorsFailedToSaveTheStudents do
        eachInvestigator \iid -> assignHorror iid attrs 1
      advanceAgendaDeck attrs
      pure a
    RequestedEncounterCard source _ (Just card) | isSource attrs source -> do
      lead <- getLead
      drawCard lead card
      pure a
    _ -> AllIsOne <$> liftRunMessage msg attrs
