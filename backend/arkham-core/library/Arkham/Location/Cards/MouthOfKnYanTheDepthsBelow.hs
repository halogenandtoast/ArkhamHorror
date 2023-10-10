module Arkham.Location.Cards.MouthOfKnYanTheDepthsBelow (
  mouthOfKnYanTheDepthsBelow,
  MouthOfKnYanTheDepthsBelow (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype MouthOfKnYanTheDepthsBelow = MouthOfKnYanTheDepthsBelow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mouthOfKnYanTheDepthsBelow :: LocationCard MouthOfKnYanTheDepthsBelow
mouthOfKnYanTheDepthsBelow =
  location
    MouthOfKnYanTheDepthsBelow
    Cards.mouthOfKnYanTheDepthsBelow
    2
    (PerPlayer 1)

instance HasAbilities MouthOfKnYanTheDepthsBelow where
  getAbilities (MouthOfKnYanTheDepthsBelow attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 (Here <> HasSupply Compass) actionAbility]

instance RunMessage MouthOfKnYanTheDepthsBelow where
  runMessage msg l@(MouthOfKnYanTheDepthsBelow attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      explorationDeck <- getExplorationDeck
      let
        deckKey = Deck.ScenarioDeckByKey ExplorationDeck
        (viewing, rest) = splitAt 2 explorationDeck
        (treacheries, other) = partition (`cardMatch` CardWithType TreacheryType) viewing
      player <- getPlayer iid
      pushAll
        [ FocusCards viewing
        , SetScenarioDeck ExplorationDeck $ other <> rest
        , chooseOne
            player
            [ Label
                "Continue"
                [ AddToEncounterDiscard c
                | c <- mapMaybe (preview _EncounterCard) treacheries
                ]
            ]
        , UnfocusCards
        , ShuffleDeck deckKey
        ]
      pure l
    _ -> MouthOfKnYanTheDepthsBelow <$> runMessage msg attrs
