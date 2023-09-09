module Arkham.Location.Cards.PathwayIntoVoid (
  pathwayIntoVoid,
  PathwayIntoVoid (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
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
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      cosmos' <- getCosmos
      pos <- findCosmosPosition iid
      let adjacents = adjacentPositions pos
          valids = filter (\adj -> isEmpty $ viewCosmos adj cosmos') adjacents
      push $ chooseOne iid []
      pure l
    RunCosmos iid lid -> do
      push $ UseCardAbility iid (toSource attrs) 1 [] NoPayment
      pure l
    _ -> PathwayIntoVoid <$> runMessage msg attrs
