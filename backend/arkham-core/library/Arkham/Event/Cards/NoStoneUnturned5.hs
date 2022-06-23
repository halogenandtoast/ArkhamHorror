module Arkham.Event.Cards.NoStoneUnturned5
  ( noStoneUnturned5
  , NoStoneUnturned5(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Message
import Arkham.Target

newtype NoStoneUnturned5 = NoStoneUnturned5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noStoneUnturned5 :: EventCard NoStoneUnturned5
noStoneUnturned5 = event NoStoneUnturned5 Cards.noStoneUnturned5

instance RunMessage NoStoneUnturned5 where
  runMessage msg e@(NoStoneUnturned5 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      iids <-
        selectList
        $ colocatedWith iid
        <> InvestigatorWithoutModifier CannotManipulateDeck

      pushAll
        [ chooseOne
          iid
          [ Search
              iid'
              (toSource attrs)
              (InvestigatorTarget iid')
              [fromDeck]
              AnyCard
              (DrawFound iid' 1)
          | iid' <- iids
          ]
        , Discard (toTarget attrs)
        ]
      pure e
    _ -> NoStoneUnturned5 <$> runMessage msg attrs
