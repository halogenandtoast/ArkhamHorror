module Arkham.Types.Event.Cards.NoStoneUnturned
  ( noStoneUnturned
  , NoStoneUnturned(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype NoStoneUnturned = NoStoneUnturned EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noStoneUnturned :: EventCard NoStoneUnturned
noStoneUnturned = event NoStoneUnturned Cards.noStoneUnturned

instance Query InvestigatorMatcher env => RunMessage env NoStoneUnturned where
  runMessage msg e@(NoStoneUnturned attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      investigatorIds <- selectList $ InvestigatorAt YourLocation
      e <$ pushAll
        [ chooseOne
          iid
          [ Search
              iid'
              (toSource attrs)
              (InvestigatorTarget iid')
              [fromTopOfDeck 6]
              []
              (DrawFound iid' 1)
          | iid' <- investigatorIds
          ]
        , Discard (toTarget attrs)
        ]
    _ -> NoStoneUnturned <$> runMessage msg attrs
