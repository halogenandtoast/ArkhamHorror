module Arkham.Location.Cards.MovingPlatformObservationStation (movingPlatformObservationStation) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid (adjacentPositions)
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WrittenInRock.Helpers (swapLocations)
import Arkham.Trait (Trait (Vault))

newtype MovingPlatformObservationStation = MovingPlatformObservationStation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

movingPlatformObservationStation :: LocationCard MovingPlatformObservationStation
movingPlatformObservationStation = location MovingPlatformObservationStation Cards.movingPlatformObservationStation 2 (Static 2)

instance HasModifiersFor MovingPlatformObservationStation where
  getModifiersFor (MovingPlatformObservationStation a) = do
    -- "Moving Platform cannot be flooded."
    modifySelf a [CannotBeFlooded]
    -- "Moving Platform is connected to each adjacent location, and vice versa."
    case locationPosition a of
      Nothing -> pure ()
      Just pos -> do
        let adjacent = mapOneOf LocationInPosition (adjacentPositions pos)
        modifySelf a [ConnectedToWhen (be a) adjacent]
        modifySelect a adjacent [ConnectedToWhen adjacent (be a)]

instance HasAbilities MovingPlatformObservationStation where
  getAbilities (MovingPlatformObservationStation a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> HasAdjacentLocations (LocationWithTrait Vault))
      $ FastAbility
      $ GroupClueCost (PerPlayer 1) (be a)

instance RunMessage MovingPlatformObservationStation where
  runMessage msg l@(MovingPlatformObservationStation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pos <- fieldJust LocationPosition attrs.id
      adjacentVault <-
        select $ mapOneOf LocationInPosition (adjacentPositions pos) <> LocationWithTrait Vault
      -- "Swap Moving Platform with an adjacent Vault location" — exchange their grid
      -- positions (and with them every card/token attached to each location).
      chooseTargetM iid adjacentVault $ swapLocations attrs.id
      pure l
    _ -> MovingPlatformObservationStation <$> liftRunMessage msg attrs
