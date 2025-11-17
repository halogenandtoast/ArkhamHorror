module Arkham.Event.Events.UnsolvedCase (unsolvedCase) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (InvestigatorEliminated)
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message (pattern RemoveClues)
import Arkham.Projection
import Arkham.Strategy

newtype UnsolvedCase = UnsolvedCase EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unsolvedCase :: EventCard UnsolvedCase
unsolvedCase = eventWith UnsolvedCase Cards.unsolvedCase $ afterPlayL .~ RemoveThisFromGame

instance HasModifiersFor UnsolvedCase where
  getModifiersFor (UnsolvedCase attrs) = case eventPlacement attrs of
    InThreatArea iid -> modified_ attrs iid [XPModifier "Unsolved Case" (-2)]
    _ -> pure ()

instance HasAbilities UnsolvedCase where
  getAbilities (UnsolvedCase a) =
    [ restricted a 1 InYourHand
        $ forced
        $ WouldBeShuffledIntoDeck (DeckIs $ HunchDeck (eventOwner a)) (CardWithId $ toCardId a)
    , restricted a 2 (InThreatAreaOf You)
        $ forced
        $ oneOf [GameEnds #when, InvestigatorEliminated #when (InvestigatorWithId $ eventOwner a)]
    ]

instance RunMessage UnsolvedCase where
  runMessage msg e@(UnsolvedCase attrs) = runQueueT $ case msg of
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid == iid' -> do
      don'tMatching \case
        ShuffleCardsIntoDeck (HunchDeck _) [card] -> card == toCard attrs
        _ -> False
      push $ CreateEventAt iid (toCard attrs) (InThreatArea iid)
      pure e
    PlayThisEvent iid (is attrs -> True) -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      highestShroud <- select $ HighestShroud Anywhere
      when (notNull highestShroud && hasClues) do
        chooseOrRunOneM iid do
          targets highestShroud \location -> do
            push $ RemoveClues (toSource attrs) (toTarget iid) 1
            push $ PlaceClues (toSource attrs) (toTarget location) 1
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- no-op, handled in HasModifiersFor
      pure e
    _ -> UnsolvedCase <$> liftRunMessage msg attrs
