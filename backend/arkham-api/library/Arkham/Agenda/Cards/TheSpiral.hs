module Arkham.Agenda.Cards.TheSpiral (theSpiral) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Draw.Types
import Arkham.Matcher
import Arkham.Scenarios.FateOfTheVale.Helpers (revealEncounterCardFromAbyss)

newtype TheSpiral = TheSpiral AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSpiral :: AgendaCard TheSpiral
theSpiral = agenda (3, A) TheSpiral Cards.theSpiral (Static 4)

instance HasAbilities TheSpiral where
  getAbilities (TheSpiral attrs) = [forcedAbility attrs 1 $ WouldDrawCard #when You EncounterDeck]

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
