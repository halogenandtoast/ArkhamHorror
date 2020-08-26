module Arkham.Types.Event
  ( allEvents
  )
where

import Arkham.Types.Card
import Arkham.Types.Event.Cards.Backstab
import Arkham.Types.Event.Cards.BlindingLight
import Arkham.Types.Event.Cards.DarkMemory
import Arkham.Types.Event.Cards.Dodge
import Arkham.Types.Event.Cards.DrawnToTheFlame
import Arkham.Types.Event.Cards.DynamiteBlast
import Arkham.Types.Event.Cards.EmergencyCache
import Arkham.Types.Event.Cards.Evidence
import Arkham.Types.Event.Cards.Lucky
import Arkham.Types.Event.Cards.MindOverMatter
import Arkham.Types.Event.Cards.OnTheLam
import Arkham.Types.Event.Cards.WardOfProtection
import Arkham.Types.Event.Cards.WorkingAHunch
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import ClassyPrelude

allEvents
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => CardCode
  -> InvestigatorId
  -> m ()
allEvents "01010" = onTheLam
allEvents "01013" = darkMemory
allEvents "01022" = evidence
allEvents "01023" = dodge
allEvents "01024" = dynamiteBlast
allEvents "01036" = mindOverMatter
allEvents "01037" = workingAHunch
allEvents "01051" = backstab
allEvents "01064" = drawnToTheFlame
allEvents "01065" = wardOfProtection
allEvents "01066" = blindingLight
allEvents "01080" = lucky
allEvents "01088" = emergencyCache
allEvents evid = const (throwString $ "No event with id: " <> show evid)
