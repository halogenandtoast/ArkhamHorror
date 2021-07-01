module Arkham.Types.Event.Cards.Shortcut
  ( shortcut
  , Shortcut(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype Shortcut = Shortcut EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shortcut :: EventCard Shortcut
shortcut = event Shortcut Cards.shortcut

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
