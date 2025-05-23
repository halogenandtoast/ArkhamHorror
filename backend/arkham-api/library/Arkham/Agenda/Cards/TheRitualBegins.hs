module Arkham.Agenda.Cards.TheRitualBegins (theRitualBegins) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait

newtype TheRitualBegins = TheRitualBegins AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualBegins :: AgendaCard TheRitualBegins
theRitualBegins = agenda (2, A) TheRitualBegins Cards.theRitualBegins (Static 5)

instance HasModifiersFor TheRitualBegins where
  getModifiersFor (TheRitualBegins attrs) =
    when (onSide A attrs) $ modifySelect attrs AnyEnemy [EnemyFight 1, EnemyEvade 1]

instance RunMessage TheRitualBegins where
  runMessage msg a@(TheRitualBegins attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid attrs iid #willpower (Fixed 6)
      advanceAgendaDeck attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      searchCollectionForRandomBasicWeakness iid attrs [Madness]
      pure a
    RequestedPlayerCard iid (isSource attrs -> True) (Just card) _ -> do
      addToHand iid (only card)
      pure a
    _ -> TheRitualBegins <$> liftRunMessage msg attrs
