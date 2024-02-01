module Arkham.Event.Cards.NoStoneUnturned5 (
  noStoneUnturned5,
  NoStoneUnturned5 (..),
) where

import Arkham.Prelude

import Arkham.Capability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype NoStoneUnturned5 = NoStoneUnturned5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

noStoneUnturned5 :: EventCard NoStoneUnturned5
noStoneUnturned5 = event NoStoneUnturned5 Cards.noStoneUnturned5

instance RunMessage NoStoneUnturned5 where
  runMessage msg e@(NoStoneUnturned5 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      iids <-
        selectList
          $ affectsOthers
          $ colocatedWith iid
          <> can.manipulate.deck

      player <- getPlayer iid
      pushAll
        [ chooseOne
            player
            [ targetLabel
              iid'
              [ search
                  iid'
                  (toSource attrs)
                  (InvestigatorTarget iid')
                  [fromDeck]
                  AnyCard
                  (DrawFound iid' 1)
              ]
            | iid' <- iids
            ]
        ]
      pure e
    _ -> NoStoneUnturned5 <$> runMessage msg attrs
