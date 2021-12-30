module Arkham.Event.Cards.Shortcut
  ( shortcut
  , Shortcut(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Id
import Arkham.Message
import Arkham.Target

newtype Shortcut = Shortcut EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shortcut :: EventCard Shortcut
shortcut = event Shortcut Cards.shortcut

instance
  ( HasQueue env
  , HasSet AccessibleLocationId env LocationId
  , HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env Shortcut where
  runMessage msg e@(Shortcut attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      lid <- getId @LocationId iid
      investigatorIds <- getSetList lid
      connectingLocations <- map unAccessibleLocationId <$> getSetList lid
      e <$ pushAll
        [ chooseOne
          iid
          [ TargetLabel
              (InvestigatorTarget iid')
              [ chooseOne
                  iid
                  [ TargetLabel
                      (LocationTarget lid')
                      [Move (toSource attrs) iid' lid lid']
                  | lid' <- connectingLocations
                  ]
              ]
          | iid' <- investigatorIds
          ]
        , Discard (toTarget attrs)
        ]
    _ -> Shortcut <$> runMessage msg attrs
