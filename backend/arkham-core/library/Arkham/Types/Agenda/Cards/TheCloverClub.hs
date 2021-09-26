module Arkham.Types.Agenda.Cards.TheCloverClub
  ( TheCloverClub(..)
  , theCloverClub
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Keyword
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype TheCloverClub = TheCloverClub AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCloverClub :: AgendaCard TheCloverClub
theCloverClub = agenda (1, A) TheCloverClub Cards.theCloverClub (Static 4)

instance Query EnemyMatcher env => HasModifiersFor env TheCloverClub where
  getModifiersFor _ (EnemyTarget eid) (TheCloverClub attrs) | onSide A attrs =
    do
      isCriminal <- member eid <$> select (EnemyWithTrait Criminal)
      pure $ toModifiers attrs [ AddKeyword Aloof | isCriminal ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities TheCloverClub where
  getAbilities (TheCloverClub x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ EnemyDealtDamage Timing.When AnyDamageEffect (EnemyWithTrait Criminal) AnySource
    | onSide A x
    ]

instance AgendaRunner env => RunMessage env TheCloverClub where
  runMessage msg a@(TheCloverClub attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAgenda $ toId attrs)
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      completedExtracurricularActivity <-
        elem "02041" . map unCompletedScenarioId <$> getSetList ()
      enemyIds <- selectList $ EnemyWithTrait Criminal

      let
        continueMessages =
          [ShuffleEncounterDiscardBackIn, NextAgenda aid "02064"]
            <> [ AdvanceCurrentAgenda | completedExtracurricularActivity ]

      a <$ pushAll
        (map EnemyCheckEngagement enemyIds
        <> [chooseOne leadInvestigatorId [Label "Continue" continueMessages]]
        )
    _ -> TheCloverClub <$> runMessage msg attrs
