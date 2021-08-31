module Arkham.Types.Agenda.Cards.RampagingCreatures
  ( RampagingCreatures(..)
  , rampagingCreatures
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher hiding (ChosenRandomLocation)
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype RampagingCreatures = RampagingCreatures AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rampagingCreatures :: AgendaCard RampagingCreatures
rampagingCreatures =
  agenda (1, A) RampagingCreatures Cards.rampagingCreatures (Static 5)

instance HasAbilities env RampagingCreatures where
  getAbilities _ _ (RampagingCreatures x) = pure
    [mkAbility x 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs EnemyPhase]

instance AgendaRunner env => RunMessage env RampagingCreatures where
  runMessage msg a@(RampagingCreatures attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      leadInvestigatorId <- getLeadInvestigatorId
      broodOfYogSothoth <- map EnemyTarget
        <$> getSetList (EnemyWithTitle "Brood of Yog-Sothoth")
      a <$ when
        (notNull broodOfYogSothoth)
        (push $ chooseOneAtATime
          leadInvestigatorId
          [ TargetLabel target [ChooseRandomLocation target mempty]
          | target <- broodOfYogSothoth
          ]
        )
    ChosenRandomLocation target@(EnemyTarget _) lid ->
      a <$ push (MoveToward target (LocationWithId lid))
    AdvanceAgenda aid | aid == agendaId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ ShuffleEncounterDiscardBackIn
        , UseScenarioSpecificAbility leadInvestigatorId Nothing 1
        , NextAgenda aid "02238"
        ]
    _ -> RampagingCreatures <$> runMessage msg attrs
