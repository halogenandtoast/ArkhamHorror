module Arkham.Location.Cards.UndergroundRiver (
  undergroundRiver,
  UndergroundRiver (..),
)
where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Control.Lens (non)

newtype UndergroundRiver = UndergroundRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundRiver :: LocationCard UndergroundRiver
undergroundRiver = locationWith UndergroundRiver Cards.undergroundRiver 4 (PerPlayer 2) connectsToAdjacent

instance HasAbilities UndergroundRiver where
  getAbilities (UndergroundRiver attrs) =
    extendRevealed attrs [mkAbility attrs 1 $ forced $ RevealLocation #after Anyone (be attrs)]

instance RunMessage UndergroundRiver where
  runMessage msg (UndergroundRiver attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure $ UndergroundRiver $ attrs & floodLevelL ?~ PartiallyFlooded
    IncreaseFloodLevel lid | lid == attrs.id -> do
      pure
        $ UndergroundRiver
        $ attrs
        & (floodLevelL . non Unflooded %~ min PartiallyFlooded . increaseFloodLevel)
    SetFloodLevel lid floodLevel | lid == attrs.id -> do
      pure $ UndergroundRiver $ attrs & floodLevelL ?~ min PartiallyFlooded floodLevel
    _ -> UndergroundRiver <$> liftRunMessage msg attrs
