module Arkham.Treachery.Cards.TimelineDestabilization
  ( timelineDestabilization
  , TimelineDestabilization(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Deck
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.SkillType
import Arkham.Target
import Arkham.Trait ( Trait (Ancient) )
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TimelineDestabilization = TimelineDestabilization TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timelineDestabilization :: TreacheryCard TimelineDestabilization
timelineDestabilization =
  treachery TimelineDestabilization Cards.timelineDestabilization

instance RunMessage TimelineDestabilization where
  runMessage msg t@(TimelineDestabilization attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- selectCount $ LocationWithTrait Ancient
      push $ RevelationSkillTest iid (toSource attrs) SkillWillpower (1 + n)
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _ -> do
      card <- field TreacheryCard (toId attrs)
      pushAll
        [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 1
        , RemoveTreachery (toId attrs)
        , ShuffleCardsIntoDeck (ScenarioDeckByKey ExplorationDeck) [card]
        ]
      pure t
    _ -> TimelineDestabilization <$> runMessage msg attrs
