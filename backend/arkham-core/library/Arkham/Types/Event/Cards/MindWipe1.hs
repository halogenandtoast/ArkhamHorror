{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.MindWipe1 where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import qualified Data.HashSet as HashSet
import Lens.Micro

import ClassyPrelude

newtype MindWipe1 = MindWipe1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mindWipe1 :: InvestigatorId -> EventId -> MindWipe1
mindWipe1 iid uuid = MindWipe1 $ baseAttrs iid uuid "01068"

instance HasActions env MindWipe1 where
  getActions i window (MindWipe1 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env MindWipe1 where
  runMessage msg (MindWipe1 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- asks (getId @LocationId iid)
      enemyIds <- HashSet.toList <$> asks (getSet locationId)
      nonEliteEnemyIds <- flip filterM enemyIds $ \enemyId -> do
        traits <- asks (getSet enemyId)
        pure $ Elite `notElem` traits

      if null nonEliteEnemyIds
        then unshiftMessage (Discard (EventTarget eventId))
        else unshiftMessages
          [ Ask iid $ ChooseOne
            [ AddModifiers (EnemyTarget eid') (EventSource eventId) [Blank]
            | eid' <- nonEliteEnemyIds
            ]
          , Discard (EventTarget eid)
          ]
      MindWipe1 <$> runMessage msg (attrs & resolved .~ True)
    _ -> MindWipe1 <$> runMessage msg attrs
