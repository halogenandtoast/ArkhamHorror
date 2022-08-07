module Arkham.Event.Cards.ThinkOnYourFeet
  ( thinkOnYourFeet
  , ThinkOnYourFeet(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Investigator.Types ( Field(..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target

newtype ThinkOnYourFeet = ThinkOnYourFeet EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thinkOnYourFeet :: EventCard ThinkOnYourFeet
thinkOnYourFeet = event ThinkOnYourFeet Cards.thinkOnYourFeet

instance RunMessage ThinkOnYourFeet where
  runMessage msg e@(ThinkOnYourFeet attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      mlid <- field InvestigatorLocation iid
      let discard = Discard (toTarget attrs)
      case mlid of
        Nothing -> push discard
        Just lid -> do
          connectedLocationIds <- selectList AccessibleLocation
          if null connectedLocationIds
             then push discard
             else pushAll
              [ chooseOne
                iid
                [ TargetLabel
                    (LocationTarget lid')
                    [Move (toSource attrs) iid lid lid']
                | lid' <- connectedLocationIds
                ]
              , discard
              ]
      pure e
    _ -> ThinkOnYourFeet <$> runMessage msg attrs
