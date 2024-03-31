module Arkham.Event.Cards.DeepKnowledge (deepKnowledge, DeepKnowledge (..)) where

import Arkham.Capability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype DeepKnowledge = DeepKnowledge EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepKnowledge :: EventCard DeepKnowledge
deepKnowledge = event DeepKnowledge Cards.deepKnowledge

instance RunMessage DeepKnowledge where
  runMessage msg e@(DeepKnowledge attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      iids <- select $ affectsOthers $ colocatedWith iid <> can.draw.cards
      player <- getPlayer iid
      choices <- for iids $ \iid' -> do
        drawing <- drawCards iid' attrs 1
        pure $ targetLabel iid' [drawing]
      pushAll
        $ replicate 3
        $ chooseOrRunOne player choices
      pure e
    _ -> DeepKnowledge <$> runMessage msg attrs
