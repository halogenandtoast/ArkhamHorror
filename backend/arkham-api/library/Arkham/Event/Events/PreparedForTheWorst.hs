module Arkham.Event.Events.PreparedForTheWorst (preparedForTheWorst) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype PreparedForTheWorst = PreparedForTheWorst EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preparedForTheWorst :: EventCard PreparedForTheWorst
preparedForTheWorst = event PreparedForTheWorst Cards.preparedForTheWorst

instance RunMessage PreparedForTheWorst where
  runMessage msg e@(PreparedForTheWorst attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      search iid attrs iid [fromTopOfDeck 9] (basic $ #asset <> #weapon) (DrawFound iid 1)
      pure e
    _ -> PreparedForTheWorst <$> liftRunMessage msg attrs
