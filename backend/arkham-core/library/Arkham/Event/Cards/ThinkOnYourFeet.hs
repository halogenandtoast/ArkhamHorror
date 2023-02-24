module Arkham.Event.Cards.ThinkOnYourFeet
  ( thinkOnYourFeet
  , ThinkOnYourFeet(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message

newtype ThinkOnYourFeet = ThinkOnYourFeet EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thinkOnYourFeet :: EventCard ThinkOnYourFeet
thinkOnYourFeet = event ThinkOnYourFeet Cards.thinkOnYourFeet

instance RunMessage ThinkOnYourFeet where
  runMessage msg e@(ThinkOnYourFeet attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      connectedLocationIds <- selectList AccessibleLocation
      unless (null connectedLocationIds) $ pushAll
        [ chooseOne
          iid
          [ TargetLabel
              (LocationTarget lid')
              [Move (toSource attrs) iid lid']
          | lid' <- connectedLocationIds
          ]
        ]
      pure e
    _ -> ThinkOnYourFeet <$> runMessage msg attrs
