module Arkham.Agenda.Cards.BidingItsTime (
  BidingItsTime (..),
  bidingItsTime,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Attack
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Creation
import Arkham.Exception
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (ChosenRandomLocation)
import Arkham.Message
import Arkham.Phase
import Arkham.Projection
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype BidingItsTime = BidingItsTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bidingItsTime :: AgendaCard BidingItsTime
bidingItsTime = agenda (2, A) BidingItsTime Cards.bidingItsTime (Static 6)

instance HasAbilities BidingItsTime where
  getAbilities (BidingItsTime x) =
    [mkAbility x 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs EnemyPhase]

instance RunMessage BidingItsTime where
  runMessage msg a@(BidingItsTime attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      lead <- getLead
      broodOfYogSothoth <- selectTargets $ EnemyWithTitle "Brood of Yog-Sothoth"
      when (notNull broodOfYogSothoth) $
        push $
          chooseOneAtATime
            lead
            [ TargetLabel target [ChooseRandomLocation target mempty]
            | target <- broodOfYogSothoth
            ]
      pure a
    ChosenRandomLocation target@(EnemyTarget _) lid -> do
      push $ MoveToward target (LocationWithId lid)
      pure a
    AdvanceAgenda aid | aid == agendaId attrs && onSide B attrs -> do
      broodOfYogSothothCount <- length <$> getSetAsideBroodOfYogSothoth
      pushAll $
        ShuffleEncounterDiscardBackIn
          : [ RequestSetAsideCard (toSource attrs) (CardCode "02255")
            | broodOfYogSothothCount > 0
            ]
            <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    RequestedSetAsideCard source card | isSource attrs source -> do
      when
        (toCardCode card /= CardCode "02255")
        (throwIO $ InvalidState "wrong card")
      lead <- getLead
      locationId <-
        fieldMap
          InvestigatorLocation
          (fromJustNote "must be somewhere")
          lead
      investigatorIds <- selectList $ colocatedWith lead
      enemyCreation <- createEnemy card locationId
      pushAll $
        toMessage enemyCreation
          : [ beginSkillTest iid source (enemyCreationEnemyId enemyCreation) SkillAgility 4
            | iid <- investigatorIds
            ]
      pure a
    FailedSkillTest iid _ (isSource attrs -> True) (SkillTestInitiatorTarget (EnemyTarget eid)) _ _ ->
      do
        push $ EnemyAttack $ enemyAttack eid attrs iid
        pure a
    _ -> BidingItsTime <$> runMessage msg attrs
