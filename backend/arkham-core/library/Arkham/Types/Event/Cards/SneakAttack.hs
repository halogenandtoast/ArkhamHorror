{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.SneakAttack where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Data.HashSet as HashSet
import Lens.Micro

import ClassyPrelude

newtype SneakAttack = SneakAttack Attrs
  deriving newtype (Show, ToJSON, FromJSON)

sneakAttack :: InvestigatorId -> EventId -> SneakAttack
sneakAttack iid uuid = SneakAttack $ baseAttrs iid uuid "01052"

instance HasActions env investigator SneakAttack where
  getActions i window (SneakAttack attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env SneakAttack where
  runMessage msg (SneakAttack attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- asks (getId @LocationId iid)
      enemyIds <- map unExhaustedEnemyId . HashSet.toList <$> asks (getSet lid)
      unshiftMessages
        $ [ EnemyDamage enemyId iid (EventSource eventId) 2
          | enemyId <- enemyIds
          ]
        <> [Discard (EventTarget eventId)]
      SneakAttack <$> runMessage msg (attrs & resolved .~ True)
    _ -> SneakAttack <$> runMessage msg attrs
