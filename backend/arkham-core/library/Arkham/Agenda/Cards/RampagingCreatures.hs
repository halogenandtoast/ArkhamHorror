module Arkham.Agenda.Cards.RampagingCreatures
  ( RampagingCreatures(..)
  , rampagingCreatures
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Types
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher hiding ( ChosenRandomLocation )
import Arkham.Message
import Arkham.Phase
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype RampagingCreatures = RampagingCreatures AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rampagingCreatures :: AgendaCard RampagingCreatures
rampagingCreatures =
  agenda (1, A) RampagingCreatures Cards.rampagingCreatures (Static 5)

instance HasAbilities RampagingCreatures where
  getAbilities (RampagingCreatures x) =
    [mkAbility x 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs EnemyPhase]

instance RunMessage RampagingCreatures where
  runMessage msg a@(RampagingCreatures attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      leadInvestigatorId <- getLeadInvestigatorId
      broodOfYogSothoth <- selectListMap EnemyTarget
        $ EnemyWithTitle "Brood of Yog-Sothoth"
      a <$ when
        (notNull broodOfYogSothoth)
        (push $ chooseOneAtATime
          leadInvestigatorId
          [ TargetLabel target [ChooseRandomLocation target mempty]
          | target <- broodOfYogSothoth
          ]
        )
    ChosenRandomLocation target@(EnemyTarget _) lid | onSide A attrs ->
      a <$ push (MoveToward target (LocationWithId lid))
    ChosenRandomLocation target lid | isTarget attrs target && onSide B attrs ->
      do
        setAsideBroodOfYogSothoth <- shuffleM =<< getSetAsideBroodOfYogSothoth
        case setAsideBroodOfYogSothoth of
          [] -> pure a
          (x : _) -> a <$ push (CreateEnemyAt x lid Nothing)
    AdvanceAgenda aid | aid == agendaId attrs && onSide B attrs -> a <$ pushAll
      [ ShuffleEncounterDiscardBackIn
      , ChooseRandomLocation (toTarget attrs) mempty
      , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
      ]
    _ -> RampagingCreatures <$> runMessage msg attrs
