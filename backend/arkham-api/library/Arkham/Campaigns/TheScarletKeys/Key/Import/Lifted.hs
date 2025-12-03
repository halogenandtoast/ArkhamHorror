module Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted (
  module X,
  module Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted,
) where

import Arkham.Calculation as X
import Arkham.Campaigns.TheScarletKeys.Key.Runner as X (
  Is (..),
  IsScarletKey,
  ScarletKeyAttrs,
  ScarletKeyCard,
  Stability (..),
  key,
  push,
  pushAll,
  pushWhen,
 )
import Arkham.Classes as X
import Arkham.Helpers.Message as X (targetLabel)
import Arkham.Message as X (
  Message (..),
  UI (..),
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
  pattern PassedThisSkillTest,
  pattern PassedThisSkillTestBy,
  pattern PlaceClues,
  pattern UseThisAbility,
 )
import Arkham.Message.Lifted as X hiding (story)
import Arkham.Prelude as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Id
import Arkham.Window

withInvestigatorBearer
  :: Applicative m => ScarletKeyAttrs -> (InvestigatorId -> m ()) -> m ()
withInvestigatorBearer attrs f = case attrs.bearer of
  InvestigatorTarget iid -> f iid
  ScenarioTarget -> traverse_ f (maybeResult @InvestigatorId attrs.meta)
  _ -> pure ()

withEnemyBearer :: Applicative m => ScarletKeyAttrs -> (EnemyId -> m ()) -> m ()
withEnemyBearer attrs f = case attrs.bearer of
  EnemyTarget eid -> f eid
  _ -> pure ()

shiftKey :: ReverseQueue m => ScarletKeyAttrs -> m () -> m ()
shiftKey attrs body = do
  checkWhen $ CampaignEvent "shiftKey" Nothing (toJSON attrs.id)
  body
  checkAfter $ CampaignEvent "shiftKey" Nothing (toJSON attrs.id)
