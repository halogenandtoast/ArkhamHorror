module Arkham.Agenda.Cards.HorrorsUnleashed (
  HorrorsUnleashed (..),
  horrorsUnleashed,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
import Arkham.Matcher hiding (ChosenRandomLocation)
import Arkham.Message
import Arkham.Modifier qualified as Modifier
import Arkham.Phase
import Arkham.Projection
import Arkham.Resolution
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype HorrorsUnleashed = HorrorsUnleashed AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrorsUnleashed :: AgendaCard HorrorsUnleashed
horrorsUnleashed =
  agenda (3, A) HorrorsUnleashed Cards.horrorsUnleashed (Static 7)

instance HasAbilities HorrorsUnleashed where
  getAbilities (HorrorsUnleashed x) =
    [mkAbility x 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs EnemyPhase]

instance HasModifiersFor HorrorsUnleashed where
  getModifiersFor (EnemyTarget eid) (HorrorsUnleashed attrs) = do
    isAbomination <- member Abomination <$> field EnemyTraits eid
    if isAbomination
      then pure $ toModifiers attrs [Modifier.EnemyFight 1, Modifier.EnemyEvade 1]
      else pure []
  getModifiersFor _ _ = pure []

instance RunMessage HorrorsUnleashed where
  runMessage msg a@(HorrorsUnleashed attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      leadInvestigatorId <- getLeadInvestigatorId
      broodOfYogSothoth <-
        selectListMap EnemyTarget $
          EnemyWithTitle "Brood of Yog-Sothoth"
      a
        <$ when
          (notNull broodOfYogSothoth)
          ( push $
              chooseOneAtATime
                leadInvestigatorId
                [ TargetLabel target [ChooseRandomLocation target mempty]
                | target <- broodOfYogSothoth
                ]
          )
    ChosenRandomLocation target@(EnemyTarget _) lid ->
      a <$ push (MoveToward target (LocationWithId lid))
    AdvanceAgenda aid
      | aid == agendaId attrs && onSide B attrs ->
          a <$ push (ScenarioResolution $ Resolution 1)
    _ -> HorrorsUnleashed <$> runMessage msg attrs
