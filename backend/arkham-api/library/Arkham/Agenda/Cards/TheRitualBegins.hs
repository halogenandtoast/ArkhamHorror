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
import Arkham.Trait

newtype TheRitualBegins = TheRitualBegins AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualBegins :: AgendaCard TheRitualBegins
theRitualBegins =
  agenda (2, A) TheRitualBegins Cards.theRitualBegins (Static 5)

instance HasModifiersFor TheRitualBegins where
  getModifiersFor (TheRitualBegins attrs) =
    if onSide A attrs
      then modifySelect attrs AnyEnemy [EnemyFight 1, EnemyEvade 1]
      else pure mempty

instance RunMessage TheRitualBegins where
  runMessage msg a@(TheRitualBegins attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      investigators <- getInvestigators
      sid <- getRandom
      pushAll
        $ [ beginSkillTest sid investigator attrs investigator #willpower (Fixed 6)
          | investigator <- investigators
          ]
        <> [advanceAgendaDeck attrs]
      pure a
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push
        $ SearchCollectionForRandom iid (toSource attrs)
        $ CardWithTrait Madness
        <> BasicWeaknessCard
      pure a
    RequestedPlayerCard iid (isSource attrs -> True) mcard _ -> do
      for_ mcard $ push . addToHand iid . PlayerCard
      pure a
    _ -> TheRitualBegins <$> runMessage msg attrs
