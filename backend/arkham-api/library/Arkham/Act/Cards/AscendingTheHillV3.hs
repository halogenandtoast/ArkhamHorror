module Arkham.Act.Cards.AscendingTheHillV3 (ascendingTheHillV3) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait

newtype AscendingTheHillV3 = AscendingTheHillV3 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV3 :: ActCard AscendingTheHillV3
ascendingTheHillV3 = act (2, A) AscendingTheHillV3 Cards.ascendingTheHillV3 Nothing

instance HasModifiersFor AscendingTheHillV3 where
  getModifiersFor (AscendingTheHillV3 attrs) = do
    modifySelect attrs Anywhere [NonTraitRestrictedModifier Altered CannotPlaceClues]

instance HasAbilities AscendingTheHillV3 where
  getAbilities (AscendingTheHillV3 x) = [mkAbility x 1 $ forced $ Enters #when You "Sentinel Peak"]

instance RunMessage AscendingTheHillV3 where
  runMessage msg a@(AscendingTheHillV3 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      sentinelPeak <- selectJust $ LocationWithTitle "Sentinel Peak"
      damage <- perPlayer 1
      createSetAsideEnemyWith_ Enemies.sethBishop sentinelPeak \x ->
        x {enemyCreationBefore = [PlaceTokens (toSource attrs) (toTarget x.enemy) #damage damage]}
      advanceActDeck attrs
      pure a
    _ -> AscendingTheHillV3 <$> liftRunMessage msg attrs
