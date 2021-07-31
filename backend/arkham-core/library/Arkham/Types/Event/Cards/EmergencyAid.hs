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

instance HasActions env EmergencyAid where
  getActions iid window (EmergencyAid attrs) = getActions iid window attrs

instance HasModifiersFor env EmergencyAid

instance
  ( HasQueue env
  , HasId LocationId env InvestigatorId
  , HasSet InvestigatorId env LocationId
  , Query AssetMatcher env
  )
  => RunMessage env EmergencyAid where
  runMessage msg e@(EmergencyAid attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      let investigatorTargets = map InvestigatorTarget investigatorIds
      allyTargets <- map AssetTarget <$> selectList
        (AssetWithTrait Ally <> AssetOneOf
          (map (AssetOwnedBy . InvestigatorWithId) investigatorIds)
        )
      e <$ push
        (chooseOne
          iid
          [ TargetLabel target [HealDamage target 2]
          | target <- investigatorTargets <> allyTargets
          ]
        )
    _ -> EmergencyAid <$> runMessage msg attrs
