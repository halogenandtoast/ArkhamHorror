module Arkham.Location.Cards.LighthouseKeepersCottage (
  lighthouseKeepersCottage,
  LighthouseKeepersCottage (..),
)
where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype LighthouseKeepersCottage = LighthouseKeepersCottage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lighthouseKeepersCottage :: LocationCard LighthouseKeepersCottage
lighthouseKeepersCottage = location LighthouseKeepersCottage Cards.lighthouseKeepersCottage 4 (PerPlayer 2)

instance HasModifiersFor LighthouseKeepersCottage where
  getModifiersFor (LighthouseKeepersCottage attrs) = modifySelf attrs [CannotBeFullyFlooded]

instance HasAbilities LighthouseKeepersCottage where
  getAbilities (LighthouseKeepersCottage a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage LighthouseKeepersCottage where
  runMessage msg l@(LighthouseKeepersCottage attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs YellowKey
      pure l
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> LighthouseKeepersCottage <$> liftRunMessage msg attrs
