module Arkham.Location.Cards.RockyShoreline (rockyShoreline) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RockyShoreline = RockyShoreline LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rockyShoreline :: LocationCard RockyShoreline
rockyShoreline = locationWith RockyShoreline Cards.rockyShoreline 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RockyShoreline where
  getAbilities (RockyShoreline a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage RockyShoreline where
  runMessage msg l@(RockyShoreline attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      crustaceanHybrid <- getSetAsideCard Enemies.crustaceanHybridInTheLight
      createEnemyAt_ crustaceanHybrid attrs
      pure l
    _ -> RockyShoreline <$> liftRunMessage msg attrs
