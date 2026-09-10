module Arkham.Homebrew.CircusExMortis.Locations.CampOutskirtsGuardedClosely (campOutskirtsGuardedClosely) where

import Arkham.Direction (Direction (..))
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CampOutskirtsGuardedClosely = CampOutskirtsGuardedClosely LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor CampOutskirtsGuardedClosely where
  getModifiersFor (CampOutskirtsGuardedClosely attrs) = do
    let adjacent = oneOf [LocationInDirection direction (be attrs) | direction <- [Above, Below, LeftOf, RightOf]]
    modifySelfWith attrs setActiveDuringSetup [ConnectedToWhen (be attrs) adjacent]
    modifySelectWith attrs adjacent setActiveDuringSetup [ConnectedToWhen adjacent (be attrs)]

campOutskirtsGuardedClosely :: LocationCard CampOutskirtsGuardedClosely
campOutskirtsGuardedClosely =
  location CampOutskirtsGuardedClosely Cards.campOutskirtsGuardedClosely 3 (Static 4)
