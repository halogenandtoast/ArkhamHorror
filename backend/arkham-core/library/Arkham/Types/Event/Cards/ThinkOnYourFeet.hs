module Arkham.Types.Event.Cards.ThinkOnYourFeet
  ( thinkOnYourFeet
  , ThinkOnYourFeet(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype ThinkOnYourFeet = ThinkOnYourFeet EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thinkOnYourFeet :: EventCard ThinkOnYourFeet
thinkOnYourFeet = event ThinkOnYourFeet Cards.thinkOnYourFeet

instance
  ( HasQueue env
  , HasSet AccessibleLocationId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env ThinkOnYourFeet where
  runMessage msg e@(ThinkOnYourFeet attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      lid <- getId @LocationId iid
      connectedLocationIds <- map unAccessibleLocationId <$> getSetList lid
      e <$ pushAll
        [ chooseOne
          iid
          [ TargetLabel
              (LocationTarget lid')
              [Move (toSource attrs) iid lid lid']
          | lid' <- connectedLocationIds
          ]
        , Discard (toTarget attrs)
        ]
    _ -> ThinkOnYourFeet <$> runMessage msg attrs
