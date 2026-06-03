module Arkham.Agenda.Cards.TheSilence (theSilence) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck

newtype TheSilence = TheSilence AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSilence :: AgendaCard TheSilence
theSilence = agenda (1, A) TheSilence Cards.theSilence (Static 6)

instance RunMessage TheSilence where
  runMessage msg a@(TheSilence attrs) = runQueueT $ case msg of
    KonamiCode pid -> do
      f <- getLogger
      selectEach (InvestigatorIsPlayer pid) \iid -> do
        liftIO $ f (ClientUI $ "theSilence:" <> tshow pid)
        drivenInsane iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sid <- getRandom
        chooseBeginSkillTest sid iid attrs iid [#willpower, #intellect] (Fixed 4)
      advanceAgendaDeckAfterSkillTest attrs
      pure a
    FailedThisSkillTestBy _ (isSource attrs -> True) n -> do
      doStep n msg
      pure a
    DoStep n inner@(FailedThisSkillTestBy iid (isSource attrs -> True) _) | n > 0 -> do
      hasCards <- fieldMap InvestigatorDeck (notNull . unDeck) iid
      chooseOneM iid do
        labeled "Take 1 horror" do
          assignHorror iid attrs 1
          doStep (n - 1) inner
        when hasCards do
          labeled "Shuffle the top card of your deck into The Abyss" do
            (map toCard -> cards, _) <- fieldMap InvestigatorDeck (draw 1) iid
            for_ (take 1 cards) \card -> do
              obtainCard card
              shuffleCardsIntoDeck (Deck.ScenarioDeckByKey AbyssDeck) [card]
            doStep (n - 1) inner
      pure a
    _ -> TheSilence <$> liftRunMessage msg attrs
