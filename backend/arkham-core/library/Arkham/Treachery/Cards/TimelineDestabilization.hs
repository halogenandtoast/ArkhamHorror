module Arkham.Treachery.Cards.TimelineDestabilization (timelineDestabilization, TimelineDestabilization (..)) where

import Arkham.Classes
import Arkham.Deck
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Trait (Trait (Ancient))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TimelineDestabilization = TimelineDestabilization TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timelineDestabilization :: TreacheryCard TimelineDestabilization
timelineDestabilization = treachery TimelineDestabilization Cards.timelineDestabilization

instance RunMessage TimelineDestabilization where
  runMessage msg t@(TimelineDestabilization attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push
        $ revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 1, CountLocations (LocationWithTrait Ancient)]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      card <- field TreacheryCard (toId attrs)
      pushAll
        [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1
        , RemoveTreachery (toId attrs)
        , ShuffleCardsIntoDeck (ScenarioDeckByKey ExplorationDeck) [card]
        ]
      pure t
    _ -> TimelineDestabilization <$> runMessage msg attrs
