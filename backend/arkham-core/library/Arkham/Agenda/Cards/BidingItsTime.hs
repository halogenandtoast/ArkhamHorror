module Arkham.Agenda.Cards.BidingItsTime
  ( BidingItsTime(..)
  , bidingItsTime
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Types
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Attack
import Arkham.Card
import Arkham.Classes
import Arkham.Exception
import Arkham.GameValue
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding ( ChosenRandomLocation )
import Arkham.Message
import Arkham.Phase
import Arkham.Projection
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.SkillType
import Arkham.Target
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
      leadInvestigatorId <- getLeadInvestigatorId
      broodOfYogSothoth <- selectListMap EnemyTarget (EnemyWithTitle "Brood of Yog-Sothoth")
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
      broodOfYogSothothCount <- length <$> getSetAsideBroodOfYogSothoth
      a <$ pushAll
        (ShuffleEncounterDiscardBackIn
        : [ RequestSetAsideCard (toSource attrs) (CardCode "02255")
          | broodOfYogSothothCount > 0
          ]
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
        )
    RequestedSetAsideCard source card | isSource attrs source -> do
      when
        (toCardCode card /= CardCode "02255")
        (throwIO $ InvalidState "wrong card")
      let enemyId = EnemyId $ toCardId card
      leadInvestigatorId <- getLeadInvestigatorId
      locationId <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be somewhere")
        leadInvestigatorId
      investigatorIds <- selectList $ colocatedWith leadInvestigatorId
      a <$ pushAll
        (CreateEnemy card
        : EnemySpawn Nothing locationId enemyId
        : [ beginSkillTest
              iid
              source
              (EnemyTarget enemyId)
              Nothing
              SkillAgility
              4
          | iid <- investigatorIds
          ]
        )
    FailedSkillTest iid _ source (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | isSource attrs source
      -> a <$ push (EnemyAttack iid eid DamageAny RegularAttack)
    _ -> BidingItsTime <$> runMessage msg attrs
