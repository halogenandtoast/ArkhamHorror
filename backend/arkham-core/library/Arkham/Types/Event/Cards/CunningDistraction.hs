{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.CunningDistraction where

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

newtype CunningDistraction = CunningDistraction Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cunningDistraction :: InvestigatorId -> EventId -> CunningDistraction
cunningDistraction iid uuid = CunningDistraction $ baseAttrs iid uuid "01078"

instance HasActions env CunningDistraction where
  getActions i window (CunningDistraction attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env CunningDistraction where
  runMessage msg (CunningDistraction attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- asks (getId @LocationId iid)
      enemyIds <- HashSet.toList <$> asks (getSet locationId)
      unshiftMessages
        $ [ EnemyEvaded iid enemyId | enemyId <- enemyIds ]
        <> [Discard (EventTarget eid)]
      CunningDistraction <$> runMessage msg (attrs & resolved .~ True)
    _ -> CunningDistraction <$> runMessage msg attrs
