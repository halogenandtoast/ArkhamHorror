{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.EmergencyAid
  ( emergencyAid
  , EmergencyAid(..)
  )
where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Trait

newtype EmergencyAid = EmergencyAid Attrs
  deriving newtype (Show, ToJSON, FromJSON)

emergencyAid :: InvestigatorId -> EventId -> EmergencyAid
emergencyAid iid uuid =
  EmergencyAid $ baseAttrs iid uuid "02105"

instance HasActions env EmergencyAid where
  getActions iid window (EmergencyAid attrs) = getActions iid window attrs

instance HasModifiersFor env EmergencyAid where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasId LocationId env InvestigatorId
  , HasSet InvestigatorId env LocationId
  , HasSet AssetId env (InvestigatorId, [Trait])
  )
  => RunMessage env EmergencyAid where
  runMessage msg e@(EmergencyAid attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      let investigatorTargets = map InvestigatorTarget investigatorIds
      allyTargets <- map AssetTarget . concat <$> for
        investigatorIds
          (getSetList . (,[Ally]))
      e <$ unshiftMessage
        (chooseOne
          iid
          [
            TargetLabel
              target
              [HealDamage target 2]
          | target <- investigatorTargets <> allyTargets
          ]
        )
    _ -> EmergencyAid <$> runMessage msg attrs
