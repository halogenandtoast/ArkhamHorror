module Arkham.Event.Events.UnbridledKnowledge5 (unbridledKnowledge5) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Projection ()
import Arkham.Search
import Arkham.Strategy
import Arkham.Zone

newtype UnbridledKnowledge5 = UnbridledKnowledge5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unbridledKnowledge5 :: EventCard UnbridledKnowledge5
unbridledKnowledge5 = event UnbridledKnowledge5 Cards.unbridledKnowledge5

instance RunMessage UnbridledKnowledge5 where
  runMessage msg e@(UnbridledKnowledge5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      clues <- iid.clues
      let n = if clues >= 2 then 8 else 5
      revealingEdit iid attrs iid (FromTopOfDeck n) \s ->
        s
          { searchZones = [(FromTopOfDeck n, PutBackInAnyOrderBothTopAndBottom)]
          , searchFoundStrategy = DrawFound iid 3
          }

      pure e
    _ -> UnbridledKnowledge5 <$> liftRunMessage msg attrs
