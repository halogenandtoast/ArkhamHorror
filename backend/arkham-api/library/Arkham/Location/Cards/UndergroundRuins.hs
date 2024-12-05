module Arkham.Location.Cards.UndergroundRuins (undergroundRuins, UndergroundRuins (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype UndergroundRuins = UndergroundRuins LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundRuins :: LocationCard UndergroundRuins
undergroundRuins =
  locationWith UndergroundRuins Cards.undergroundRuins 2 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasModifiersFor UndergroundRuins where
  getModifiersFor (UndergroundRuins a) =
    whenRevealed a $ modifySelfWhenM a (selectAny $ enemyAt a) [InVictoryDisplayForCountingVengeance]

instance HasAbilities UndergroundRuins where
  getAbilities (UndergroundRuins a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ EnemyLeavesPlay #after $ enemyAt a

instance RunMessage UndergroundRuins where
  runMessage msg l@(UndergroundRuins attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> UndergroundRuins <$> liftRunMessage msg attrs
