module Arkham.Event.Events.SmuggledGoods (smuggledGoods) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Zone

newtype SmuggledGoods = SmuggledGoods EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smuggledGoods :: EventCard SmuggledGoods
smuggledGoods = event SmuggledGoods Cards.smuggledGoods

instance RunMessage SmuggledGoods where
  runMessage msg e@(SmuggledGoods attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      hasIllicitCardInDiscard <- selectAny $ inDiscardOf iid <> basic #illicit
      chooseOrRunOneM iid do
        labeled "Search deck" do
          search iid attrs iid [fromTopOfDeck 9] (basic #illicit) (DrawFound iid 1)
          shuffleIntoDeck iid attrs
        when hasIllicitCardInDiscard do
          labeled "Search discard" do
            search iid attrs iid [(FromDiscard, PutBack)] (basic #illicit) (DrawFound iid 1)
      pure e
    _ -> SmuggledGoods <$> liftRunMessage msg attrs
