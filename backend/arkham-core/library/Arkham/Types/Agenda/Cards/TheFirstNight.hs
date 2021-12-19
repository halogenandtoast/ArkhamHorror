module Arkham.Types.Agenda.Cards.TheFirstNight (
  TheFirstNight,
  theFirstNight,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Scenarios.APhantomOfTruth.Helpers
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Matcher hiding (MoveAction)
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype TheFirstNight = TheFirstNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFirstNight :: AgendaCard TheFirstNight
theFirstNight = agenda (1, A) TheFirstNight Cards.theFirstNight (Static 12)

instance HasRecord env () => HasModifiersFor env TheFirstNight where
  getModifiersFor _ target (TheFirstNight a) | not (isTarget a target) = do
    conviction <- getRecordCount Conviction
    doubt <- getRecordCount Doubt
    pure $ toModifiers a $ [DoomSubtracts | conviction > doubt]
  getModifiersFor _ _ _ = pure []

instance AgendaRunner env => RunMessage env TheFirstNight where
  runMessage msg a@(TheFirstNight attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      iids <- getInvestigatorIds
      enemyPairs <-
        traverse
          (traverseToSnd (selectList . EnemyIsEngagedWith . InvestigatorWithId))
          iids
      locationPairs <-
        traverse
          ( traverseToSnd
              ( selectList
                  . AccessibleFrom
                  . LocationWithInvestigator
                  . InvestigatorWithId
              )
          )
          iids
      pushAll $
        [ DisengageEnemy iid enemy
        | (iid, enemies) <- enemyPairs
        , enemy <- enemies
        ]
          <> [ chooseOneAtATime
                leadInvestigatorId
                [ TargetLabel
                  (InvestigatorTarget iid)
                  [ chooseOne
                      iid
                      [ TargetLabel
                        (LocationTarget lid)
                        [MoveAction iid lid Free False]
                      | lid <- locations
                      ]
                  ]
                | (iid, locations) <- locationPairs
                ]
             ]
          <> [NextAdvanceAgendaStep (toId attrs) 2]
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    NextAdvanceAgendaStep aid 2 | aid == toId attrs && onSide B attrs -> do
      organist <- selectJust $ EnemyOneOf [enemyIs Cards.theOrganistDrapedInMystery, enemyIs Cards.theOrganistHopelessIDefiedHim]
      from <- getId organist
      leadInvestigatorId <- getLeadInvestigatorId
      (minDistance, iids) <- investigatorsNearestToTheOrganist
      lids <- setFromList . concat <$> for iids (\iid -> do
        rs <- getList iid
        pure $ map fst $ filter ((> minDistance) . snd) rs)
      withNoInvestigators <- select LocationWithoutInvestigators
      let
        forced = lids `intersect` withNoInvestigators
        targets = toList $ if null forced then lids else forced
      push $ chooseOne leadInvestigatorId [TargetLabel (LocationTarget lid) [EnemyMove organist from lid] | lid <- targets]
      pure a
    _ -> TheFirstNight <$> runMessage msg attrs
