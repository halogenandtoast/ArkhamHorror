module Arkham.Event.Cards.UnsolvedCase
  ( unsolvedCase
  , UnsolvedCase(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Projection
import Arkham.Target

newtype UnsolvedCase = UnsolvedCase EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unsolvedCase :: EventCard UnsolvedCase
unsolvedCase = event UnsolvedCase Cards.unsolvedCase

instance HasAbilities UnsolvedCase where
  getAbilities (UnsolvedCase a) =
    [ restrictedAbility a 1 InYourHand $ ForcedAbility $ WouldBeShuffledIntoDeck
        (DeckIs HunchDeck)
        (CardWithId $ toCardId a)
    ]

instance RunMessage UnsolvedCase where
  runMessage msg e@(UnsolvedCase attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      popMessageMatching_ $ \case
        ShuffleCardsIntoDeck HunchDeck [card] -> card == toCard attrs
        _ -> False
      push $ PlaceEvent (toId e) (InThreatArea iid)
      pure e
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      highestShroud <- selectList $ HighestShroud Anywhere
      pushAll
        $ [ chooseOrRunOne
              iid
              [ targetLabel
                  location
                  [ RemoveClues (InvestigatorTarget iid) 1
                  , PlaceClues (LocationTarget location) 1
                  ]
              | location <- highestShroud
              ]
          | notNull highestShroud && hasClues
          ]
        <> [RemoveFromGame (toTarget attrs)]
      pure e
    _ -> UnsolvedCase <$> runMessage msg attrs
