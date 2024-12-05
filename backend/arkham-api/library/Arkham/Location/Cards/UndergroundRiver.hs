module Arkham.Location.Cards.UndergroundRiver (undergroundRiver, UndergroundRiver (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype UndergroundRiver = UndergroundRiver LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundRiver :: LocationCard UndergroundRiver
undergroundRiver = locationWith UndergroundRiver Cards.undergroundRiver 4 (PerPlayer 2) connectsToAdjacent

instance HasModifiersFor UndergroundRiver where
  getModifiersFor (UndergroundRiver a) = whenRevealed a $ modifySelf a [CannotBeFullyFlooded]

instance HasAbilities UndergroundRiver where
  getAbilities (UndergroundRiver attrs) =
    extendRevealed attrs [mkAbility attrs 1 $ forced $ RevealLocation #after Anyone (be attrs)]

instance RunMessage UndergroundRiver where
  runMessage msg l@(UndergroundRiver attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      increaseThisFloodLevelTo attrs PartiallyFlooded
      pure l
    _ -> UndergroundRiver <$> liftRunMessage msg attrs
