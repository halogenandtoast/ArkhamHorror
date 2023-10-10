module Arkham.Act.Cards.IntoTheRuinsOnceAgain (
  IntoTheRuinsOnceAgain (..),
  intoTheRuinsOnceAgain,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Ability
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype IntoTheRuinsOnceAgain = IntoTheRuinsOnceAgain ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheRuinsOnceAgain :: ActCard IntoTheRuinsOnceAgain
intoTheRuinsOnceAgain =
  act
    (1, A)
    IntoTheRuinsOnceAgain
    Cards.intoTheRuinsOnceAgain
    (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance HasAbilities IntoTheRuinsOnceAgain where
  getAbilities (IntoTheRuinsOnceAgain a) =
    withBaseAbilities
      a
      [ restrictedAbility a 1 (ScenarioDeckWithCard ExplorationDeck)
          $ ActionAbility (Just Action.Explore)
          $ ActionCost 1
      ]

instance RunMessage IntoTheRuinsOnceAgain where
  runMessage msg a@(IntoTheRuinsOnceAgain attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push
        $ Explore
          iid
          source
          (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      chamberOfTime <- getSetAsideCard Locations.chamberOfTime
      pushAll
        [ ShuffleCardsIntoDeck
            (Deck.ScenarioDeckByKey ExplorationDeck)
            [chamberOfTime]
        , AddToVictory (toTarget attrs)
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> IntoTheRuinsOnceAgain <$> runMessage msg attrs
