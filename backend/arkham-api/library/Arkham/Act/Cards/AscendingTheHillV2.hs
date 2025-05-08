module Arkham.Act.Cards.AscendingTheHillV2 (ascendingTheHillV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait

newtype AscendingTheHillV2 = AscendingTheHillV2 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV2 :: ActCard AscendingTheHillV2
ascendingTheHillV2 = act (2, A) AscendingTheHillV2 Cards.ascendingTheHillV2 Nothing

instance HasModifiersFor AscendingTheHillV2 where
  getModifiersFor (AscendingTheHillV2 attrs) = do
    modifySelect attrs Anywhere [NonTraitRestrictedModifier Altered CannotPlaceClues]

instance HasAbilities AscendingTheHillV2 where
  getAbilities (AscendingTheHillV2 x) = [mkAbility x 1 $ forced $ Enters #when You "Sentinel Peak"]

instance RunMessage AscendingTheHillV2 where
  runMessage msg a@(AscendingTheHillV2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      sentinelPeak <- selectJust $ LocationWithTitle "Sentinel Peak"
      createEnemyAt_ Enemies.sethBishop sentinelPeak
      advanceActDeck attrs
      pure a
    _ -> AscendingTheHillV2 <$> liftRunMessage msg attrs
