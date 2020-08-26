{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Elusive where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Data.HashSet as HashSet
import Lens.Micro

import ClassyPrelude

newtype Elusive = Elusive Attrs
  deriving newtype (Show, ToJSON, FromJSON)

elusive :: InvestigatorId -> EventId -> Elusive
elusive iid uuid = Elusive $ baseAttrs iid uuid "01050"

instance HasActions env investigator Elusive where
  getActions i window (Elusive attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Elusive where
  runMessage msg (Elusive attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      enemyIds <- HashSet.toList <$> asks (getSet iid)
      emptyLocations <- HashSet.map unEmptyLocationId <$> asks (getSet ())
      revealedLocations <- HashSet.map unRevealedLocationId <$> asks (getSet ())
      let
        candidateLocations =
          HashSet.toList $ emptyLocations `intersection` revealedLocations

      unshiftMessages
        $ [ DisengageEnemy iid enemyId | enemyId <- enemyIds ]
        <> [ Ask iid $ ChooseOne [ MoveTo iid lid | lid <- candidateLocations ]
           | not (null candidateLocations)
           ]
        <> [Discard (EventTarget eventId)]

      Elusive <$> runMessage msg (attrs & resolved .~ True)
    _ -> Elusive <$> runMessage msg attrs
