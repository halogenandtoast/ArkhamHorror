module Arkham.Homebrew.AgesUnwound.Agendas.ArmyOfOne (armyOfOne) where

import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorDeck))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype ArmyOfOne = ArmyOfOne AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armyOfOne :: AgendaCard ArmyOfOne
armyOfOne = agenda (2, A) ArmyOfOne Cards.armyOfOne (Static 6)

instance RunMessage ArmyOfOne where
  runMessage msg a@(ArmyOfOne attrs) = runQueueT $ scenarioI18n $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      {- "Each investigator must either take 2 horror or choose and discard 3 cards
      from their hand." -}
      eachInvestigator \iid -> chooseOneM iid $ scope "armyOfOne" do
        countVar 2 $ labeled "takeHorror" $ assignHorror iid attrs 2
        countVar 3 $ labeled "discardCards" $ chooseAndDiscardCards iid attrs 3

      {- "Each player searches the top 6 cards of another player's deck for 2
      non-weakness, non-signature cards and draws them, then shuffles the searched
      deck." With one investigator there is no other player's deck to search; the
      campaign's solo dummy deck (guide p2) is out of scope for a digital table. -}
      iids <- allInvestigators
      for_ iids \iid -> do
        let others = filter (/= iid) iids
        unless (null others) do
          chooseOrRunOneM iid $ targets others \other -> do
            top6 <- fieldMap InvestigatorDeck (take 6 . (.cards)) other
            let eligible =
                  filter (\c -> toCard c `cardMatch` (NonWeakness <> not_ SignatureCard)) top6
            unless (null eligible) do
              focusCards eligible do
                chooseNM iid (min 2 (length eligible))
                  $ cardsLabeled eligible (drawCardFrom iid (Deck.InvestigatorDeck other))
            shuffleDeck other

      advanceAgendaDeck attrs
      pure a
    _ -> ArmyOfOne <$> liftRunMessage msg attrs
