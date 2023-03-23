module Arkham.Event.Cards.SleightOfHand
  ( sleightOfHand
  , SleightOfHand(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Cost.Status qualified as Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait

newtype SleightOfHand = SleightOfHand EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleightOfHand :: EventCard SleightOfHand
sleightOfHand = event SleightOfHand Cards.sleightOfHand

instance RunMessage SleightOfHand where
  runMessage msg e@(SleightOfHand attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      cards <- selectList
        $ PlayableCard Cost.PaidCost
        $ InHandOf (InvestigatorWithId iid) <> BasicCardMatch (CardWithTrait Item)
      push $ chooseOne
        iid
        [ targetLabel
          (toCardId card)
          [ PutCardIntoPlay iid card (Just $ toTarget attrs) windows'
          , CreateEffect
            (toCardCode attrs)
            Nothing
            (toSource attrs)
            (toTarget $ toCardId card)
          ]
        | card <- cards
        ]
      pure e
    _ -> SleightOfHand <$> runMessage msg attrs
