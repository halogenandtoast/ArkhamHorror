module Arkham.Event.Events.LucidDreaming2 (lucidDreaming2, LucidDreaming2 (..)) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype LucidDreaming2 = LucidDreaming2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucidDreaming2 :: EventCard LucidDreaming2
lucidDreaming2 = event LucidDreaming2 Cards.lucidDreaming2

instance RunMessage LucidDreaming2 where
  runMessage msg e@(LucidDreaming2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      canRevealCards <- can.reveal.cards iid
      cards <- select $ oneOf $ inPlayAreaOf iid : [inHandOf iid | canRevealCards]
      chooseTargetM iid cards \card ->
        search iid attrs iid [fromDeck] (basic $ CardWithTitle card.title) (DrawFound iid 1)
      pure e
    _ -> LucidDreaming2 <$> liftRunMessage msg attrs
