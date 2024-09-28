module Arkham.Location.Cards.NewChurchGreenInTooDeep (
  newChurchGreenInTooDeep,
  NewChurchGreenInTooDeep (..),
)
where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers

newtype NewChurchGreenInTooDeep = NewChurchGreenInTooDeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newChurchGreenInTooDeep :: LocationCard NewChurchGreenInTooDeep
newChurchGreenInTooDeep =
  locationWith
    NewChurchGreenInTooDeep
    Cards.newChurchGreenInTooDeep
    3
    (PerPlayer 1)
    connectsToAdjacent

instance HasAbilities NewChurchGreenInTooDeep where
  getAbilities (NewChurchGreenInTooDeep a) =
    extendRevealed
      a
      [ restricted a 1 UnrevealedKeyIsSetAside $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 (Here <> thisIs a LocationWithoutClues) parleyAction_
      ]

instance RunMessage NewChurchGreenInTooDeep where
  runMessage msg l@(NewChurchGreenInTooDeep attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeUnrevealedKeyOn attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    _ -> NewChurchGreenInTooDeep <$> liftRunMessage msg attrs
