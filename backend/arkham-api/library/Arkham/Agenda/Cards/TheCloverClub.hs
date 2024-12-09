module Arkham.Agenda.Cards.TheCloverClub (
  TheCloverClub (..),
  theCloverClub,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Campaign
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype TheCloverClub = TheCloverClub AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCloverClub :: AgendaCard TheCloverClub
theCloverClub = agenda (1, A) TheCloverClub Cards.theCloverClub (Static 4)

instance HasModifiersFor TheCloverClub where
  getModifiersFor (TheCloverClub attrs) =
    if onSide A attrs
      then modifySelect attrs (EnemyWithTrait Criminal) [AddKeyword Aloof]
      else pure mempty

instance HasAbilities TheCloverClub where
  getAbilities (TheCloverClub x) =
    [ mkAbility x 1
      $ ForcedAbility
      $ EnemyDealtDamage
        Timing.When
        AnyDamageEffect
        (EnemyWithTrait Criminal)
        AnySource
    | onSide A x
    ]

instance RunMessage TheCloverClub where
  runMessage msg a@(TheCloverClub attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push (AdvanceAgenda $ toId attrs)
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      completedExtracurricularActivity <-
        elem "02041" <$> getCompletedScenarios
      enemyIds <- select $ EnemyWithTrait Criminal

      let
        continueMessages =
          [ ShuffleEncounterDiscardBackIn
          , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
          ]
            <> [AdvanceCurrentAgenda | completedExtracurricularActivity]

      pushAll
        $ map EnemyCheckEngagement enemyIds
        <> [chooseOne lead [Label "Continue" continueMessages]]
      pure a
    _ -> TheCloverClub <$> runMessage msg attrs
