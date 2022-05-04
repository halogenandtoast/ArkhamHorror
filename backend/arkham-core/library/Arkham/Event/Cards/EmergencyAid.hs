module Arkham.Event.Cards.EmergencyAid
  ( emergencyAid
  , EmergencyAid(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Trait

newtype EmergencyAid = EmergencyAid EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyAid :: EventCard EmergencyAid
emergencyAid = event EmergencyAid Cards.emergencyAid

instance
  ( HasQueue env
  , HasSet InvestigatorId env InvestigatorMatcher
  , Query AssetMatcher env
  )
  => RunMessage env EmergencyAid where
  runMessage msg e@(EmergencyAid attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      investigatorIds <- getSetList
        (InvestigatorAt YourLocation <> InvestigatorWithAnyDamage)
      let investigatorTargets = map InvestigatorTarget investigatorIds
      allyTargets <- map AssetTarget <$> selectList
        (AssetWithDamage <> AssetWithTrait Ally <> AssetOneOf
          (map (AssetControlledBy . InvestigatorWithId) investigatorIds)
        )
      e <$ pushAll
        (chooseOne
            iid
            [ TargetLabel target [HealDamage target 2]
            | target <- investigatorTargets <> allyTargets
            ]
        : [Discard (toTarget attrs)]
        )
    _ -> EmergencyAid <$> runMessage msg attrs
