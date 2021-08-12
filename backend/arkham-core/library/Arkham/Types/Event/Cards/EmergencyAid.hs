module Arkham.Types.Event.Cards.EmergencyAid
  ( emergencyAid
  , EmergencyAid(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype EmergencyAid = EmergencyAid EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyAid :: EventCard EmergencyAid
emergencyAid = event EmergencyAid Cards.emergencyAid

instance HasActions EmergencyAid
instance HasModifiersFor env EmergencyAid

instance
  ( HasQueue env
  , HasSet InvestigatorId env InvestigatorMatcher
  , Query AssetMatcher env
  )
  => RunMessage env EmergencyAid where
  runMessage msg e@(EmergencyAid attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      investigatorIds <- getSetList
        (InvestigatorAt YourLocation <> InvestigatorWithAnyDamage)
      let investigatorTargets = map InvestigatorTarget investigatorIds
      allyTargets <- map AssetTarget <$> selectList
        (AssetWithDamage <> AssetWithTrait Ally <> AssetOneOf
          (map (AssetOwnedBy . InvestigatorWithId) investigatorIds)
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
