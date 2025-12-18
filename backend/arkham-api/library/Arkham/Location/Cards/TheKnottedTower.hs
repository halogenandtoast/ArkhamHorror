module Arkham.Location.Cards.TheKnottedTower (theKnottedTower) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TheKnottedTower = TheKnottedTower LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKnottedTower :: LocationCard TheKnottedTower
theKnottedTower = locationWith TheKnottedTower Cards.theKnottedTower 3 (Static 0) connectsToAdjacent

instance HasAbilities TheKnottedTower where
  getAbilities (TheKnottedTower a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction (ScenarioEvent #after (Just You) "exposedAdjacentLocation")

getExposedLocation :: [Window] -> LocationId
getExposedLocation = \case
  [] -> error "missing exposed location"
  ((windowType -> Window.ScenarioEvent "exposedAdjacentLocation" _ value) : _) -> toResult value
  (_ : xs) -> getExposedLocation xs

instance RunMessage TheKnottedTower where
  runMessage msg l@(TheKnottedTower attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      moveTo (attrs.ability 1) iid (getExposedLocation ws)
      pure l
    _ -> TheKnottedTower <$> liftRunMessage msg attrs
