module Arkham.Treachery.Cards.MergingTimelinesMachinationsThroughTime (
  mergingTimelinesMachinationsThroughTime,
) where

import Arkham.Card
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Name
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MergingTimelinesMachinationsThroughTime = MergingTimelinesMachinationsThroughTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mergingTimelinesMachinationsThroughTime :: TreacheryCard MergingTimelinesMachinationsThroughTime
mergingTimelinesMachinationsThroughTime =
  treachery MergingTimelinesMachinationsThroughTime Cards.mergingTimelinesMachinationsThroughTime

instance RunMessage MergingTimelinesMachinationsThroughTime where
  runMessage msg t@(MergingTimelinesMachinationsThroughTime attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      discardTopOfDeckAndHandle iid attrs 5 attrs
      pure t
    DiscardedTopOfDeck iid cards (isSource attrs -> True) (isTarget attrs -> True) -> do
      let titles = map toTitle cards
      hand <- filter ((`elem` titles) . toTitle) <$> iid.hand
      for_ hand \card -> do
        discardCard iid attrs card
        loseActions iid attrs 1
      shuffleCardsIntoDeck iid $ filterCards WeaknessCard (map PlayerCard cards <> hand)
      pure t
    _ -> MergingTimelinesMachinationsThroughTime <$> liftRunMessage msg attrs
