module Arkham.Types.Agenda.Cards.BidingItsTime
  ( BidingItsTime(..)
  , bidingItsTime
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Exception
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype BidingItsTime = BidingItsTime AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bidingItsTime :: AgendaCard BidingItsTime
bidingItsTime = agenda (2, A) BidingItsTime Cards.bidingItsTime (Static 6)

instance HasModifiersFor env BidingItsTime

-- TODO: Forced ability
instance HasActions BidingItsTime

instance AgendaRunner env => RunMessage env BidingItsTime where
  runMessage msg a@(BidingItsTime attrs) = case msg of
    EndEnemy -> do
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
      broodOfYogSothothCount <- unSetAsideCount
        <$> getCount @SetAsideCount (CardCode "02255")
      a <$ pushAll
        (ShuffleEncounterDiscardBackIn
        : [ RequestSetAsideCard (toSource attrs) (CardCode "02255")
          | broodOfYogSothothCount > 0
          ]
        <> [NextAgenda aid "02239"]
        )
    RequestedSetAsideCard source card | isSource attrs source -> do
      when
        (toCardCode card /= CardCode "02255")
        (throwIO $ InvalidState "wrong card")
      let enemyId = EnemyId $ toCardId card
      leadInvestigatorId <- getLeadInvestigatorId
      locationId <- getId leadInvestigatorId
      investigatorIds <- getSetList locationId
      a <$ pushAll
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
      -> a <$ push (EnemyAttack iid eid DamageAny)
    _ -> BidingItsTime <$> runMessage msg attrs
