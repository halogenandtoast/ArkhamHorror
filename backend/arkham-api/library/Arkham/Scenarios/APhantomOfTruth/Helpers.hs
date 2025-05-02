module Arkham.Scenarios.APhantomOfTruth.Helpers (
  module Arkham.Scenarios.APhantomOfTruth.Helpers,
  module X,
) where

import Arkham.Campaigns.ThePathToCarcosa.Helpers as X
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Prelude
import Arkham.Source

getTheOrganist :: HasGame m => m EnemyId
getTheOrganist = selectJust $ EnemyWithTitle "The Organist"

withTheOrganist :: HasGame m => (EnemyId -> m ()) -> m ()
withTheOrganist f = getTheOrganist >>= f

moveOrganistAwayFromNearestInvestigator :: ReverseQueue m => m ()
moveOrganistAwayFromNearestInvestigator = do
  withTheOrganist \organist -> do
    lead <- getLead
    investigators <- select $ NearestToEnemy (be organist)
    start <- selectJust $ locationWithEnemy organist

    lids <-
      concatForM investigators $ getMaybeLocation >=> \case
        Nothing -> pure []
        Just lid ->
          select
            $ LocationFartherFromMatching lid start
            $ ConnectedTo (locationWithEnemy organist)
    emptyLids <- filterM (<=~> LocationWithoutInvestigators) lids

    let locations = if notNull emptyLids then emptyLids else lids
    chooseOrRunOneM lead do
      questionLabeled "Move the organist"
      targets locations $ enemyMoveTo organist

disengageEachEnemyAndMoveToConnectingLocation
  :: (ReverseQueue m, Sourceable source) => source -> m ()
disengageEachEnemyAndMoveToConnectingLocation source = do
  lead <- getLead
  investigators <- getInvestigators
  chooseOneAtATimeM lead do
    targets investigators \iid -> do
      locations <- select $ ConnectedFrom $ locationWithInvestigator iid
      enemies <- select $ enemyEngagedWith iid
      for_ enemies (disengageEnemy iid)
      chooseOneM iid do
        labeled "Do not move" nothing
        targets locations (moveTo source iid)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "aPhantomOfTruth" a
