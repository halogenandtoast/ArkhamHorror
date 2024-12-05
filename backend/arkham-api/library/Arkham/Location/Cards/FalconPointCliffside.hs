module Arkham.Location.Cards.FalconPointCliffside (falconPointCliffside, FalconPointCliffside (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ALightInTheFog.Helpers
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype FalconPointCliffside = FalconPointCliffside LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falconPointCliffside :: LocationCard FalconPointCliffside
falconPointCliffside = location FalconPointCliffside Cards.falconPointCliffside 1 (PerPlayer 1)

instance HasModifiersFor FalconPointCliffside where
  getModifiersFor (FalconPointCliffside attrs) = modifySelf attrs [CannotBeFullyFlooded]

instance HasAbilities FalconPointCliffside where
  getAbilities (FalconPointCliffside attrs) =
    extendRevealed1 attrs
      $ groupLimit PerGame
      $ restricted attrs 1 (Here <> youExist (InvestigatorWithKey WhiteKey))
      $ FastAbility Free

instance RunMessage FalconPointCliffside where
  runMessage msg l@(FalconPointCliffside attrs) = runQueueT $ case msg of
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flashback iid Flashback12
      pure l
    _ -> FalconPointCliffside <$> liftRunMessage msg attrs
