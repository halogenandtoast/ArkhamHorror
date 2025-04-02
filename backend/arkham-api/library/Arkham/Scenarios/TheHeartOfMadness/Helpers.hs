module Arkham.Scenarios.TheHeartOfMadness.Helpers where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Classes.HasQueue
import Arkham.I18n
import Arkham.Target
import Arkham.Prelude
import Arkham.Id
import Arkham.Message
import Arkham.Message.Lifted.Queue

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "toTheForbiddenPeaks" a

sealAtLocationOf :: Applicative m => InvestigatorId -> m Bool
sealAtLocationOf _iid = pure False

placeSeal :: (ReverseQueue m, Targetable target) => target -> Seal -> m ()
placeSeal target = push . PlaceSeal (toTarget target)

activateSeal :: ReverseQueue m => SealKind -> m ()
activateSeal = push . ActivateSeal
