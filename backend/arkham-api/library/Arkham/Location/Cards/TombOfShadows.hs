module Arkham.Location.Cards.TombOfShadows (tombOfShadows, TombOfShadows (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TombOfShadows = TombOfShadows LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tombOfShadows :: LocationCard TombOfShadows
tombOfShadows =
  locationWith TombOfShadows Cards.tombOfShadows 4 (PerPlayer 2)
    $ (connectsToL .~ adjacentLocations)
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasModifiersFor TombOfShadows where
  getModifiersFor (TombOfShadows a) = do
    modifySelect a (enemyIs Enemies.theManInThePallidMask <> enemyAt a) [HealthModifier 1]

instance HasAbilities TombOfShadows where
  getAbilities (TombOfShadows attrs) =
    extendRevealed1 attrs $ mkAbility attrs 1 $ forced $ RevealLocation #when Anyone (be attrs)

instance RunMessage TombOfShadows where
  runMessage msg l@(TombOfShadows attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      actIds <- select AnyAct
      pushAll $ map (\aid -> AdvanceAct aid (attrs.ability 1) #other) actIds
      pure l
    _ -> TombOfShadows <$> liftRunMessage msg attrs
