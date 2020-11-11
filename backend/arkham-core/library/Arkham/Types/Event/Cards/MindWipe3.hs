{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.MindWipe3 where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Trait

newtype MindWipe3 = MindWipe3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

mindWipe3 :: InvestigatorId -> EventId -> MindWipe3
mindWipe3 iid uuid = MindWipe3 $ baseAttrs iid uuid "50008"

instance HasModifiersFor env MindWipe3 where
  getModifiersFor = noModifiersFor

instance HasActions env MindWipe3 where
  getActions i window (MindWipe3 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env MindWipe3 where
  runMessage msg (MindWipe3 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- asks (getId @LocationId iid)
      enemyIds <- asks $ setToList . getSet locationId
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
