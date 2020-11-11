{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.DynamiteBlast where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype DynamiteBlast = DynamiteBlast Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dynamiteBlast :: InvestigatorId -> EventId -> DynamiteBlast
dynamiteBlast iid uuid = DynamiteBlast $ baseAttrs iid uuid "01023"

instance HasModifiersFor env DynamiteBlast where
  getModifiersFor = noModifiersFor

instance HasActions env DynamiteBlast where
  getActions i window (DynamiteBlast attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env DynamiteBlast where
  runMessage msg (DynamiteBlast attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      currentLocationId <- asks (getId @LocationId iid)
      connectedLocationIds <-
        asks $ map unConnectedLocationId . setToList . getSet currentLocationId
      choices <- for (currentLocationId : connectedLocationIds) $ \lid -> do
        enemyIds <- asks $ setToList . getSet lid
        investigatorIds <- asks $ setToList . getSet @InvestigatorId lid
        pure
          $ map (\enid -> EnemyDamage enid iid (EventSource eid) 3) enemyIds
          <> map
               (\iid' -> InvestigatorAssignDamage iid' (EventSource eid) 3 0)
               investigatorIds
      let availableChoices = filter (not . null) choices
      unshiftMessages
        [ Ask iid $ ChooseOne $ map Run availableChoices
        , Discard (EventTarget eid)
        ]
      DynamiteBlast <$> runMessage msg (attrs & resolved .~ True)
    _ -> DynamiteBlast <$> runMessage msg attrs
