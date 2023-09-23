module Arkham.Agenda.Cards.TheRitualBegins (
  TheRitualBegins (..),
  theRitualBegins,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait

newtype TheRitualBegins = TheRitualBegins AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualBegins :: AgendaCard TheRitualBegins
theRitualBegins =
  agenda (2, A) TheRitualBegins Cards.theRitualBegins (Static 5)

instance HasModifiersFor TheRitualBegins where
  getModifiersFor (EnemyTarget _) (TheRitualBegins attrs) | onSide A attrs = do
    pure $ toModifiers attrs [EnemyFight 1, EnemyEvade 1]
  getModifiersFor _ _ = pure []

instance RunMessage TheRitualBegins where
  runMessage msg a@(TheRitualBegins attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      investigators <- getInvestigators
      pushAll
        $ [beginSkillTest investigator attrs investigator #willpower 6 | investigator <- investigators]
        <> [advanceAgendaDeck attrs]
      pure a
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push
        $ SearchCollectionForRandom iid (toSource attrs)
        $ CardWithType PlayerTreacheryType
        <> CardWithTrait Madness
        <> CardWithSubType BasicWeakness
      pure a
    RequestedPlayerCard iid (isSource attrs -> True) mcard _ -> do
      for_ mcard $ push . addToHand iid . PlayerCard
      pure a
    _ -> TheRitualBegins <$> runMessage msg attrs
