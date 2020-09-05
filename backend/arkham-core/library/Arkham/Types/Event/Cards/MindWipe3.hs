{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.MindWipe3 where

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

newtype MindWipe3 = MindWipe3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mindWipe3 :: InvestigatorId -> EventId -> MindWipe3
mindWipe3 iid uuid = MindWipe3 $ baseAttrs iid uuid "50008"

instance HasActions env investigator MindWipe3 where
  getActions i window (MindWipe3 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env MindWipe3 where
  runMessage msg (MindWipe3 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      locationId <- asks (getId @LocationId iid)
      enemyIds <- HashSet.toList <$> asks (getSet locationId)
      nonEliteEnemyIds <- flip filterM enemyIds $ \enemyId -> do
        traits <- asks (getSet enemyId)
        pure $ Elite `notElem` traits

      if null nonEliteEnemyIds
        then unshiftMessage (Discard (EventTarget eventId))
        else unshiftMessages
          [ Ask iid $ ChooseOne
            [ AddModifiers
                (EnemyTarget eid')
                (EventSource eventId)
                [Blank, DamageDealt (-1), HorrorDealt (-1)]
            | eid' <- nonEliteEnemyIds
            ]
          , Discard (EventTarget eid)
          ]
      MindWipe3 <$> runMessage msg (attrs & resolved .~ True)
    _ -> MindWipe3 <$> runMessage msg attrs
