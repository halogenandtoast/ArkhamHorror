module Arkham.Event.Cards.UnsolvedCase (unsolvedCase, UnsolvedCase (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection

newtype UnsolvedCase = UnsolvedCase EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unsolvedCase :: EventCard UnsolvedCase
unsolvedCase = eventWith UnsolvedCase Cards.unsolvedCase $ afterPlayL .~ RemoveThisFromGame

instance HasModifiersFor UnsolvedCase where
  getModifiersFor (InvestigatorTarget iid) (UnsolvedCase attrs) = case eventPlacement attrs of
    InThreatArea iid' | iid == iid' -> pure $ toModifiers attrs [XPModifier (-2)]
    _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities UnsolvedCase where
  getAbilities (UnsolvedCase a) =
    [ restrictedAbility a 1 InYourHand
        $ forced
        $ WouldBeShuffledIntoDeck (DeckIs $ HunchDeck (eventOwner a)) (CardWithId $ toCardId a)
    , restrictedAbility a 2 (InThreatAreaOf You)
        $ forced
        $ oneOf [GameEnds #when, InvestigatorEliminated #when (InvestigatorWithId $ eventOwner a)]
    ]

instance RunMessage UnsolvedCase where
  runMessage msg e@(UnsolvedCase attrs) = case msg of
    InHand iid' (UseCardAbility iid (isSource attrs -> True) 1 _ _) | iid == iid' -> do
      popMessageMatching_ $ \case
        ShuffleCardsIntoDeck (HunchDeck _) [card] ->
          card == toCard attrs
        _ -> False
      push $ CreateEventAt iid (toCard attrs) (InThreatArea iid)
      pure e
    PlayThisEvent iid eid | eid == toId attrs -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      highestShroud <- selectList $ HighestShroud Anywhere
      player <- getPlayer iid
      pushAll
        [ chooseOrRunOne
          player
          [ targetLabel
            location
            [ RemoveClues (toSource attrs) (toTarget iid) 1
            , PlaceClues (toSource attrs) (toTarget location) 1
            ]
          | location <- highestShroud
          ]
        | notNull highestShroud && hasClues
        ]
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- no-op, handled in HasModifiersFor
      pure e
    _ -> UnsolvedCase <$> runMessage msg attrs
