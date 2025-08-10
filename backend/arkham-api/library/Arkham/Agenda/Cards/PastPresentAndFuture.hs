module Arkham.Agenda.Cards.PastPresentAndFuture (pastPresentAndFuture) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Log (getRecordSet)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype PastPresentAndFuture = PastPresentAndFuture AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pastPresentAndFuture :: AgendaCard PastPresentAndFuture
pastPresentAndFuture = agenda (2, A) PastPresentAndFuture Cards.pastPresentAndFuture (Static 4)

instance HasAbilities PastPresentAndFuture where
  getAbilities (PastPresentAndFuture x) =
    [ mkAbility x 1 $ forced $ MovedBy #after You Matcher.EncounterCardSource
    | onSide A x
    ]

instance RunMessage PastPresentAndFuture where
  runMessage msg a@(PastPresentAndFuture attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      lead <- getLead
      discardUntilFirst lead attrs Deck.EncounterDeck (basic #location)

      sacrificedToYogSothoth <- getRecordSet SacrificedToYogSothoth
      unless (null sacrificedToYogSothoth) do
        eachInvestigator \iid -> do
          sid <- getRandom
          beginSkillTest sid iid attrs iid #willpower (Fixed $ length sacrificedToYogSothoth)
      advanceAgendaDeck attrs
      pure a
    RequestedEncounterCard (isSource attrs -> True) _ (Just card) -> do
      lead <- getLead
      resolveRevelation lead card
      pure a
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignDamage iid attrs n
      pure a
    _ -> PastPresentAndFuture <$> liftRunMessage msg attrs
