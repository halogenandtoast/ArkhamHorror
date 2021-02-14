module Arkham.Types.Agenda.Cards.BidingItsTime
  ( BidingItsTime(..)
  , bidingItsTime
  )
where

import Arkham.Prelude

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.EnemyMatcher
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Query
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
      leadInvestigatorId <- getLeadInvestigatorId
      locationId <- getId leadInvestigatorId
      a <$ unshiftMessages
        (ShuffleEncounterDiscardBackIn
        : [ UseScenarioSpecificAbility
              leadInvestigatorId
              (Just (LocationTarget locationId))
              1
          | broodOfYogSothothCount > 0
          ]
        <> [NextAgenda aid "02239"]
        )
    _ -> BidingItsTime <$> runMessage msg attrs
