module Arkham.Location.Cards.PathwayIntoVoid (
  pathwayIntoVoid,
  PathwayIntoVoid (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Scenario.Deck
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers

newtype PathwayIntoVoid = PathwayIntoVoid LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathwayIntoVoid :: LocationCard PathwayIntoVoid
pathwayIntoVoid =
  locationWith
    PathwayIntoVoid
    Cards.pathwayIntoVoid
    4
    (Static 2)
    (connectsToL .~ adjacentLocations)

instance HasAbilities PathwayIntoVoid where
  getAbilities (PathwayIntoVoid attrs) =
    withRevealedAbilities attrs [cosmos attrs 1]

instance RunMessage PathwayIntoVoid where
  runMessage msg l@(PathwayIntoVoid attrs) = case msg of
    RunCosmos iid lid msgs | lid == toId attrs -> do
      cosmos' <- getCosmos
      pos <- findCosmosPosition iid
      let adjacents = adjacentPositions pos
          valids = filter (\adj -> isEmpty $ viewCosmos adj cosmos') adjacents
      if null valids
        then
          pushAll
            [ RemoveFromGame (toTarget lid)
            , ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) [toCard attrs]
            ]
        else
          push
            $ chooseOne
              iid
              [GridLabel (cosmicLabel pos') (PlaceCosmos iid (toId attrs) x y : msgs) | pos'@(Pos x y) <- valids]
      pure l
    _ -> PathwayIntoVoid <$> runMessage msg attrs
