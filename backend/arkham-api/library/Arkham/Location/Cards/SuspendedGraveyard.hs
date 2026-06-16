module Arkham.Location.Cards.SuspendedGraveyard (suspendedGraveyard) where

import Arkham.Ability
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck

newtype SuspendedGraveyard = SuspendedGraveyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspendedGraveyard :: LocationCard SuspendedGraveyard
suspendedGraveyard = locationWith SuspendedGraveyard Cards.suspendedGraveyard 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities SuspendedGraveyard where
  getAbilities (SuspendedGraveyard a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> ScenarioDeckWithCard CavernsDeck
            <> exists (mapOneOf (`LocationWithSpaceInDirection` be a) [minBound ..])
        )
      $ actionAbilityWithCost (HorrorCost (a.ability 1) YouTarget 1)

instance RunMessage SuspendedGraveyard where
  runMessage msg l@(SuspendedGraveyard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getScenarioDeck CavernsDeck >>= \case
        [] -> pure ()
        (card : rest) -> do
          setScenarioDeck CavernsDeck rest
          pos <- fieldJust LocationPosition attrs.id
          emptyPositions <- filterM (selectNone . LocationInPosition) pos.adjacents
          chooseOrRunOneM iid do
            for_ emptyPositions \emptyPos ->
              gridLabeled_ emptyPos $ placeLocationInGrid_ emptyPos card
      pure l
    _ -> SuspendedGraveyard <$> liftRunMessage msg attrs
