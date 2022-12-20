module Arkham.Event.Cards.EmergencyAid
  ( emergencyAid
  , EmergencyAid(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype EmergencyAid = EmergencyAid EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyAid :: EventCard EmergencyAid
emergencyAid = event EmergencyAid Cards.emergencyAid

instance RunMessage EmergencyAid where
  runMessage msg e@(EmergencyAid attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      investigatorIds <- selectList
        (InvestigatorAt YourLocation <> InvestigatorWithAnyDamage)
      let investigatorTargets = map InvestigatorTarget investigatorIds
      allyTargets <- selectListMap AssetTarget
        (AssetWithDamage <> AllyAsset <> AssetOneOf
          (map (AssetControlledBy . InvestigatorWithId) investigatorIds)
        )
      e <$ pushAll
        (chooseOne
            iid
            [ TargetLabel target [HealDamage target (toSource attrs) 2]
            | target <- investigatorTargets <> allyTargets
            ]
        : [Discard (toTarget attrs)]
        )
    _ -> EmergencyAid <$> runMessage msg attrs
