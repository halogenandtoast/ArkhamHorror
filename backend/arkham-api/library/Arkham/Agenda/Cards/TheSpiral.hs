module Arkham.Agenda.Cards.TheSpiral (theSpiral) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck

newtype TheSpiral = TheSpiral AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSpiral :: AgendaCard TheSpiral
theSpiral = agenda (3, A) TheSpiral Cards.theSpiral (Static 4)

instance HasAbilities TheSpiral where
  getAbilities (TheSpiral attrs) = [forcedAbility attrs 1 $ WouldDrawCard #when You EncounterDeck]

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
          withI18n $ labeled' "continue" do
            unfocusCards
            for_ revealed \card -> scenarioSpecific "removeFromAbyss" (toCardId card)
            for_ (reverse nonEncounter) \card -> push $ PutCardOnTopOfDeck iid (Deck.ScenarioDeckByKey AbyssDeck) card
            push $ DrewCards iid $ finalizeDraw (newCardDraw source Deck.EncounterDeck 1) [EncounterCard ec]
    _ -> pure ()
  when (null rest && notNull nonEncounter) do
    focusCards nonEncounter do
      chooseOneM iid do
        withI18n $ labeled' "continue" do
          unfocusCards
          for_ nonEncounter \card -> scenarioSpecific "removeFromAbyss" (toCardId card)
          for_ (reverse nonEncounter) \card -> push $ PutCardOnTopOfDeck iid (Deck.ScenarioDeckByKey AbyssDeck) card

instance RunMessage TheSpiral where
  runMessage msg a@(TheSpiral attrs) = runQueueT $ case msg of
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
      eachInvestigator $ kill attrs
      pure a
    _ -> TheSpiral <$> liftRunMessage msg attrs
