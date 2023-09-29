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
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCloverClub :: AgendaCard TheCloverClub
theCloverClub = agenda (1, A) TheCloverClub Cards.theCloverClub (Static 4)

instance HasModifiersFor TheCloverClub where
  getModifiersFor (EnemyTarget eid) (TheCloverClub attrs) | onSide A attrs =
    do
      isCriminal <- member eid <$> select (EnemyWithTrait Criminal)
      pure $ toModifiers attrs [AddKeyword Aloof | isCriminal]
  getModifiersFor _ _ = pure []

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
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          a <$ push (AdvanceAgenda $ toId attrs)
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      completedExtracurricularActivity <-
        elem "02041" <$> getCompletedScenarios
      enemyIds <- selectList $ EnemyWithTrait Criminal

      let
        continueMessages =
          [ ShuffleEncounterDiscardBackIn
          , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
          ]
            <> [AdvanceCurrentAgenda | completedExtracurricularActivity]

      a
        <$ pushAll
          ( map EnemyCheckEngagement enemyIds
              <> [chooseOne leadInvestigatorId [Label "Continue" continueMessages]]
          )
    _ -> TheCloverClub <$> runMessage msg attrs
