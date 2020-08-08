module Arkham.Types.Event
  ( allEvents
  )
where

import Arkham.Types.Card
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.Event.Cards.EmergencyCache
import Arkham.Types.Event.Cards.Evidence
import Arkham.Types.Event.Cards.Dodge
import Arkham.Types.Event.Cards.DynamiteBlast
import Arkham.Types.Event.Cards.MindOverMatter
import Arkham.Types.Event.Cards.WorkingAHunch
import Arkham.Types.Event.Cards.WardOfProtection
import Arkham.Types.Event.Cards.BlindingLight
import ClassyPrelude

allEvents
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => CardCode
  -> InvestigatorId
  -> m ()
allEvents "01022" = evidence
allEvents "01023" = dodge
allEvents "01024" = dynamiteBlast
allEvents "01036" = mindOverMatter
allEvents "01037" = workingAHunch
allEvents "01065" = wardOfProtection
allEvents "01066" = blindingLight
allEvents "01088" = emergencyCache
allEvents evid = const (throwString $ "No event with id: " <> show evid)
