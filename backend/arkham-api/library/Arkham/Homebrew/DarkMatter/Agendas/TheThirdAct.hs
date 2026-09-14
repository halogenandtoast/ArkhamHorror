module Arkham.Homebrew.DarkMatter.Agendas.TheThirdAct (theThirdAct) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.MachineInYellow
import Arkham.Matcher
import Arkham.Strategy

newtype TheThirdAct = TheThirdAct AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThirdAct :: AgendaCard TheThirdAct
theThirdAct = agenda (1, A) TheThirdAct Cards.theThirdAct (Static 4)

instance HasAbilities TheThirdAct where
  getAbilities (TheThirdAct a) = [memoriesInsteadOfHorror a]

instance RunMessage TheThirdAct where
  runMessage msg a@(TheThirdAct attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      crossOffMemoriesInsteadOfHorror iid ws
      pure a
    {- Agenda 1b:

    "Each investigator may search their deck and discard pile for a player card
    and draw it.
    Each investigator must search their deck and discard pile for a weakness and
    draw it (signature if possible.)" -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid ->
        search iid attrs iid [fromDeck, fromDiscard] #any (DrawFoundUpTo iid 1)
      eachInvestigator \iid -> do
        let signature = #weakness <> SignatureCard
        hasSignature <-
          orM
            [ selectAny $ InDeckOf (InvestigatorWithId iid) <> basic signature
            , selectAny $ InDiscardOf (InvestigatorWithId iid) <> basic signature
            ]
        search iid attrs iid [fromDeck, fromDiscard] (basic $ if hasSignature then signature else #weakness)
          $ DrawFound iid 1
      advanceAgendaDeck attrs
      pure a
    _ -> TheThirdAct <$> liftRunMessage msg attrs
