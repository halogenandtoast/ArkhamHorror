module Arkham.Types.Agenda.Cards.BidingItsTime
  ( BidingItsTime(..)
  , bidingItsTime
  ) where

import Arkham.Prelude

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.EnemyMatcher
import Arkham.Types.Exception
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype BidingItsTime = BidingItsTime AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bidingItsTime :: BidingItsTime
bidingItsTime =
  BidingItsTime $ baseAttrs "02238" "Biding Its Time" (Agenda 2 A) (Static 6)

instance HasModifiersFor env BidingItsTime where
  getModifiersFor = noModifiersFor

instance HasActions env BidingItsTime where
  getActions i window (BidingItsTime x) = getActions i window x

instance AgendaRunner env => RunMessage env BidingItsTime where
  runMessage msg a@(BidingItsTime attrs) = case msg of
    EndEnemy -> do
      leadInvestigatorId <- getLeadInvestigatorId
      broodOfYogSothoth <- map EnemyTarget
        <$> getSetList (EnemyWithTitle "Brood of Yog-Sothoth")
      a <$ unshiftMessage
        (chooseOneAtATime
          leadInvestigatorId
          [ TargetLabel target [ChooseRandomLocation target mempty]
          | target <- broodOfYogSothoth
          ]
        )
    ChosenRandomLocation target@(EnemyTarget _) lid ->
      a <$ unshiftMessage (MoveToward target (LocationWithId lid))
    AdvanceAgenda aid | aid == agendaId attrs && onSide B attrs -> do
      broodOfYogSothothCount <- unSetAsideCount
        <$> getCount @SetAsideCount (CardCode "02255")
      a <$ unshiftMessages
        (ShuffleEncounterDiscardBackIn
        : [ RequestSetAsideCard (toSource attrs) (CardCode "02255")
          | broodOfYogSothothCount > 0
          ]
        <> [NextAgenda aid "02239"]
        )
    RequestedSetAsideCard source card | isSource attrs source -> do
      when
        (card ^. defL . cardCodeL /= CardCode "02255")
        (throwIO $ InvalidState "wrong card")
      let enemyId = EnemyId $ card ^. cardIdL
      leadInvestigatorId <- getLeadInvestigatorId
      locationId <- getId leadInvestigatorId
      investigatorIds <- getSetList locationId
      a <$ unshiftMessages
        (CreateEnemy card
        : EnemySpawn Nothing locationId enemyId
        : [ BeginSkillTest
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
      -> a <$ unshiftMessage (EnemyAttack iid eid)
    _ -> BidingItsTime <$> runMessage msg attrs
