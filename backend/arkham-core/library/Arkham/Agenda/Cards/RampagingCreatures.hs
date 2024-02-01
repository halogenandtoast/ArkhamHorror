module Arkham.Agenda.Cards.RampagingCreatures (
  RampagingCreatures (..),
  rampagingCreatures,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher hiding (ChosenRandomLocation)
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers

newtype RampagingCreatures = RampagingCreatures AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

rampagingCreatures :: AgendaCard RampagingCreatures
rampagingCreatures = agenda (1, A) RampagingCreatures Cards.rampagingCreatures (Static 5)

instance HasAbilities RampagingCreatures where
  getAbilities (RampagingCreatures x) = [mkAbility x 1 $ ForcedAbility $ PhaseEnds #when #enemy]

instance RunMessage RampagingCreatures where
  runMessage msg a@(RampagingCreatures attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLeadPlayer
      broodOfYogSothoth <- selectTargets $ EnemyWithTitle "Brood of Yog-Sothoth"
      pushWhen (notNull broodOfYogSothoth)
        $ chooseOneAtATime lead
        $ targetLabels broodOfYogSothoth (\target -> only $ ChooseRandomLocation target mempty)
      pure a
    ChosenRandomLocation target@(EnemyTarget _) lid | onSide A attrs -> do
      push $ MoveToward target (LocationWithId lid)
      pure a
    ChosenRandomLocation target lid | isTarget attrs target && onSide B attrs -> do
      setAsideBroodOfYogSothoth <- shuffleM =<< getSetAsideBroodOfYogSothoth
      for_ (nonEmpty setAsideBroodOfYogSothoth) $ \(x :| _) ->
        pushM $ createEnemyAt_ x lid Nothing
      pure a
    AdvanceAgenda aid | aid == agendaId attrs && onSide B attrs -> do
      pushAll
        [ ShuffleEncounterDiscardBackIn
        , ChooseRandomLocation (toTarget attrs) mempty
        , advanceAgendaDeck attrs
        ]
      pure a
    _ -> RampagingCreatures <$> runMessage msg attrs
