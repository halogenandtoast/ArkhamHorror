module Arkham.Treachery.Cards.TimelineDestabilization (timelineDestabilization) where

import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Trait (Trait (Ancient))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TimelineDestabilization = TimelineDestabilization TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timelineDestabilization :: TreacheryCard TimelineDestabilization
timelineDestabilization = treachery TimelineDestabilization Cards.timelineDestabilization

instance RunMessage TimelineDestabilization where
  runMessage msg t@(TimelineDestabilization attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 1, CountLocations (LocationWithTrait Ancient)]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      card <- field TreacheryCard (toId attrs)
      assignDamageAndHorror iid attrs 1 1
      removeTreachery attrs
      shuffleCardsIntoDeck ExplorationDeck [card]
      pure t
    _ -> TimelineDestabilization <$> liftRunMessage msg attrs
