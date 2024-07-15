module Arkham.Event.Cards.EndOfTheRoad (endOfTheRoad, EndOfTheRoad (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Strategy

newtype EndOfTheRoad = EndOfTheRoad EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endOfTheRoad :: EventCard EndOfTheRoad
endOfTheRoad = eventWith EndOfTheRoad Cards.endOfTheRoad (afterPlayL .~ RemoveThisFromGame)

instance RunMessage EndOfTheRoad where
  runMessage msg e@(EndOfTheRoad attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      drawCardsIfCan iid attrs 1
      gainResourcesIfCan iid attrs 1
      gainActions iid attrs 1
      pure e
    _ -> EndOfTheRoad <$> liftRunMessage msg attrs
