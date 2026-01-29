module Arkham.Event.Events.ForMyNextTrick (forMyNextTrick) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Cost.Status
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Item, Spell))

newtype ForMyNextTrick = ForMyNextTrick EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forMyNextTrick :: EventCard ForMyNextTrick
forMyNextTrick = event ForMyNextTrick Cards.forMyNextTrick

instance RunMessage ForMyNextTrick where
  runMessage msg e@(ForMyNextTrick attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      search
        iid
        attrs
        iid
        [fromDeck]
        (PlayableCardWithCostReduction NoAction 2 $ basic (#asset <> mapOneOf CardWithTrait [Spell, Item]))
        (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ xs -> do
      chooseTargetM iid xs \card -> do
        reduceCostOf attrs card 2
        playCardPayingCost iid card
      pure e
    _ -> ForMyNextTrick <$> liftRunMessage msg attrs
