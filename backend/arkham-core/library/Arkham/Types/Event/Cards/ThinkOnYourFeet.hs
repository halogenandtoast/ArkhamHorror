module Arkham.Types.Event.Cards.ThinkOnYourFeet
  ( thinkOnYourFeet
  , ThinkOnYourFeet(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Event.Attrs

newtype ThinkOnYourFeet = ThinkOnYourFeet EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thinkOnYourFeet :: InvestigatorId -> EventId -> ThinkOnYourFeet
thinkOnYourFeet iid uuid = ThinkOnYourFeet $ baseAttrs iid uuid "02025"

instance HasActions env ThinkOnYourFeet where
  getActions iid window (ThinkOnYourFeet attrs) = getActions iid window attrs

instance HasModifiersFor env ThinkOnYourFeet where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasSet AccessibleLocationId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env ThinkOnYourFeet where
  runMessage msg e@(ThinkOnYourFeet attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- getId @LocationId iid
      connectedLocationIds <- map unAccessibleLocationId <$> getSetList lid
      e <$ unshiftMessages
        [ chooseOne
          iid
          [ TargetLabel (LocationTarget lid') [Move iid lid lid']
          | lid' <- connectedLocationIds
          ]
        , Discard (toTarget attrs)
        ]
    _ -> ThinkOnYourFeet <$> runMessage msg attrs
