module Arkham.Location.Cards.LanternRoom (lanternRoom, LanternRoom (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype LanternRoom = LanternRoom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lanternRoom :: LocationCard LanternRoom
lanternRoom = location LanternRoom Cards.lanternRoom 2 (PerPlayer 2)

instance HasModifiersFor LanternRoom where
  getModifiersFor (LanternRoom attrs) = modifySelf attrs [CannotBeFlooded]

instance HasAbilities LanternRoom where
  getAbilities (LanternRoom a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage LanternRoom where
  runMessage msg l@(LanternRoom attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs WhiteKey
      pure l
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> LanternRoom <$> liftRunMessage msg attrs
