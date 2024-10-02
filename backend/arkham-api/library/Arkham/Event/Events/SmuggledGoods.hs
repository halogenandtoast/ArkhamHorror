module Arkham.Event.Events.SmuggledGoods (smuggledGoods, SmuggledGoods (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Strategy
import Arkham.Trait
import Arkham.Zone

newtype SmuggledGoods = SmuggledGoods EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

smuggledGoods :: EventCard SmuggledGoods
smuggledGoods = event SmuggledGoods Cards.smuggledGoods

instance RunMessage SmuggledGoods where
  runMessage msg e@(SmuggledGoods attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      hasIllicitCardInDiscard <- fieldP InvestigatorDiscard (any (member Illicit . toTraits)) iid
      chooseOrRunOneM iid do
        labeled "Search deck" do
          search iid attrs iid [fromTopOfDeck 9] (basic #illicit) (DrawFound iid 1)
          shuffleIntoDeck iid attrs
        when hasIllicitCardInDiscard do
          labeled "Search discard" do
            search iid attrs attrs [(FromDiscard, PutBack)] (basic #illicit) (DrawFound iid 1)
      pure e
    _ -> SmuggledGoods <$> liftRunMessage msg attrs
