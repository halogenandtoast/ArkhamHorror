module Arkham.Act.Cards.WorldsBeyond (
  WorldsBeyond (..),
  worldsBeyond,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Timing qualified as Timing

newtype WorldsBeyond = WorldsBeyond ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

worldsBeyond :: ActCard WorldsBeyond
worldsBeyond =
  act
    (1, A)
    WorldsBeyond
    Cards.worldsBeyond
    (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance HasAbilities WorldsBeyond where
  getAbilities (WorldsBeyond a) =
    withBaseAbilities a
      $ [ mkAbility a 1
            $ ForcedAbility
            $ EnemyAttemptsToSpawnAt
              Timing.When
              AnyEnemy
              LocationNotInPlay
        ]

instance RunMessage WorldsBeyond where
  runMessage msg a@(WorldsBeyond attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      aPocketInTime <- getSetAsideCard Locations.aPocketInTime
      pushAll
        [ ShuffleCardsIntoDeck
            (ScenarioDeckByKey ExplorationDeck)
            [aPocketInTime]
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      explorationDeck <- getExplorationDeck
      lead <- getLeadPlayer
      let
        (nonMatched, remaining) =
          break (`cardMatch` CardWithType LocationType) explorationDeck
      case remaining of
        [] ->
          pushAll
            [ FocusCards nonMatched
            , chooseOne
                lead
                [ Label
                    "No locations found"
                    [UnfocusCards, ShuffleDeck (ScenarioDeckByKey ExplorationDeck)]
                ]
            ]
        (x : _) -> do
          placement <- placeLocation_ x
          pushAll
            [ FocusCards (nonMatched <> [x])
            , chooseOne
                lead
                [ TargetLabel
                    (CardIdTarget $ toCardId x)
                    [ RemoveCardFromScenarioDeck ExplorationDeck x
                    , placement
                    , UnfocusCards
                    , ShuffleDeck (ScenarioDeckByKey ExplorationDeck)
                    ]
                ]
            ]
      pure a
    _ -> WorldsBeyond <$> runMessage msg attrs
