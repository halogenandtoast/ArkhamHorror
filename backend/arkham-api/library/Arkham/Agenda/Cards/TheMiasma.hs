module Arkham.Agenda.Cards.TheMiasma (theMiasma) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types
import Arkham.Helpers
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.FateOfTheVale.Helpers (scenarioI18n)

newtype TheMiasma = TheMiasma AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMiasma :: AgendaCard TheMiasma
theMiasma = agenda (2, A) TheMiasma Cards.theMiasma (Static 5)

instance HasAbilities TheMiasma where
  getAbilities (TheMiasma attrs) = [forcedAbility attrs 1 $ WouldDrawCard #when You EncounterDeck]

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

instance RunMessage TheMiasma where
  runMessage msg a@(TheMiasma attrs) = runQueueT $ case msg of
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
        chooseBeginSkillTest sid iid attrs iid [#combat, #agility] (Fixed 4)
      advanceAgendaDeckAfterSkillTest attrs
      pure a
    FailedThisSkillTestBy _ (isSource attrs -> True) n -> do
      doStep n msg
      pure a
    DoStep n inner@(FailedThisSkillTestBy iid (isSource attrs -> True) _) | n > 0 -> do
      hasCards <- fieldMap InvestigatorDeck (notNull . unDeck) iid
      chooseOneM iid $ scenarioI18n do
        unscoped $ countVar 1 $ labeled' "takeDamage" do
          assignDamage iid attrs 1
          doStep (n - 1) inner
        when hasCards do
          labeled' "theMiasma.shuffleTopCardIntoAbyss" do
            (map toCard -> cards, _) <- fieldMap InvestigatorDeck (draw 1) iid
            for_ (take 1 cards) \card -> do
              obtainCard card
              shuffleCardsIntoDeck (Deck.ScenarioDeckByKey AbyssDeck) [card]
            doStep (n - 1) inner
      pure a
    _ -> TheMiasma <$> liftRunMessage msg attrs
