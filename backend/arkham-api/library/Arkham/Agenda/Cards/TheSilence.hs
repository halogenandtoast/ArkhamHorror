module Arkham.Agenda.Cards.TheSilence (theSilence) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types
import Arkham.Helpers
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck

newtype TheSilence = TheSilence AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSilence :: AgendaCard TheSilence
theSilence = agenda (1, A) TheSilence Cards.theSilence (Static 6)

instance HasAbilities TheSilence where
  getAbilities (TheSilence attrs) =
    [forcedAbility attrs 1 $ WouldDrawCard #when You EncounterDeck]

revealEncounterCardFromAbyss :: ReverseQueue m => Source -> InvestigatorId -> m ()
revealEncounterCardFromAbyss source iid = do
  abyss <- getScenarioDeck AbyssDeck
  let
    hasEncounterBack = \case
      EncounterCard c -> not (cdDoubleSided $ toCardDef c)
      _ -> False
    (nonEncounter, rest) = break hasEncounterBack (reverse abyss)
  case rest of
    EncounterCard ec : _ -> do
      let revealed = nonEncounter <> [EncounterCard ec]
      focusCards revealed do
        chooseOneM iid do
          labeled "Continue" do
            unfocusCards
            for_ revealed \card -> scenarioSpecific "removeFromAbyss" (toCardId card)
            for_ (reverse nonEncounter) \card -> push $ PutCardOnTopOfDeck iid Deck.EncounterDeck card
            push $ DrewCards iid $ finalizeDraw (newCardDraw source Deck.EncounterDeck 1) [EncounterCard ec]
    _ -> pure ()
  when (null rest && notNull nonEncounter) do
    focusCards nonEncounter do
      chooseOneM iid do
        labeled "Continue" do
          unfocusCards
          for_ nonEncounter \card -> scenarioSpecific "removeFromAbyss" (toCardId card)
          for_ (reverse nonEncounter) \card -> push $ PutCardOnTopOfDeck iid Deck.EncounterDeck card

instance RunMessage TheSilence where
  runMessage msg a@(TheSilence attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ ReplaceCurrentCardDraw iid $ newCardDraw (attrs.ability 1) Deck.EncounterDeck 0
      revealEncounterCardFromAbyss (attrs.ability 1) iid
      pure a
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
