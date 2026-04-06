module Arkham.Event.Events.Respite2 (respite2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype Respite2 = Respite2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

respite2 :: EventCard Respite2
respite2 = event Respite2 Cards.respite2

instance RunMessage Respite2 where
  runMessage msg e@(Respite2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- select $ inDiscardOf iid <> basic (oneOf [#event, #skill])
      chooseUpToNM_ iid 3 $ targets cards $ shuffleCardsIntoDeck iid . only
      drawCards iid attrs 1
      pure e
    _ -> Respite2 <$> liftRunMessage msg attrs
