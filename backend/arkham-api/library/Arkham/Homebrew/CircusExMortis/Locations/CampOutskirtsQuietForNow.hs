module Arkham.Homebrew.CircusExMortis.Locations.CampOutskirtsQuietForNow (campOutskirtsQuietForNow) where

import Arkham.Direction (Direction (..))
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CampOutskirtsQuietForNow = CampOutskirtsQuietForNow LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor CampOutskirtsQuietForNow where
  getModifiersFor (CampOutskirtsQuietForNow attrs) = do
    let adjacent = oneOf [LocationInDirection direction (be attrs) | direction <- [Above, Below, LeftOf, RightOf]]
    modifySelfWith attrs setActiveDuringSetup [ConnectedToWhen (be attrs) adjacent]
    modifySelectWith attrs adjacent setActiveDuringSetup [ConnectedToWhen adjacent (be attrs)]

campOutskirtsQuietForNow :: LocationCard CampOutskirtsQuietForNow
campOutskirtsQuietForNow =
  location CampOutskirtsQuietForNow Cards.campOutskirtsQuietForNow 3 (PerPlayer 3)
