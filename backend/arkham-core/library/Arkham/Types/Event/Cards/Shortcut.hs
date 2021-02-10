module Arkham.Types.Event.Cards.Shortcut
  ( shortcut
  , Shortcut(..)
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

newtype Shortcut = Shortcut EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shortcut :: InvestigatorId -> EventId -> Shortcut
shortcut iid uuid = Shortcut $ baseAttrs iid uuid "02022"

instance HasActions env Shortcut where
  getActions iid window (Shortcut attrs) = getActions iid window attrs

instance HasModifiersFor env Shortcut where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasSet AccessibleLocationId env LocationId
  , HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env Shortcut where
  runMessage msg e@(Shortcut attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- getId @LocationId iid
      investigatorIds <- getSetList lid
      connectingLocations <- map unAccessibleLocationId <$> getSetList lid
      e <$ unshiftMessages
        [ chooseOne
          iid
          [ TargetLabel
              (InvestigatorTarget iid')
              [ chooseOne
                  iid
                  [ TargetLabel (LocationTarget lid') [Move iid' lid lid']
                  | lid' <- connectingLocations
                  ]
              ]
          | iid' <- investigatorIds
          ]
        , Discard (toTarget attrs)
        ]
    _ -> Shortcut <$> runMessage msg attrs
