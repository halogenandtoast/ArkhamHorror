module Arkham.Location.Cards.ArkhamWoodsWoodenBridge where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (arkhamWoodsWoodenBridge)
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype ArkhamWoodsWoodenBridge = ArkhamWoodsWoodenBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsWoodenBridge :: LocationCard ArkhamWoodsWoodenBridge
arkhamWoodsWoodenBridge = location ArkhamWoodsWoodenBridge Cards.arkhamWoodsWoodenBridge 3 (PerPlayer 1)

instance HasAbilities ArkhamWoodsWoodenBridge where
  getAbilities (ArkhamWoodsWoodenBridge a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> DuringSkillTest (WhileEvadingAnEnemy AnyEnemy))
      $ forced
      $ RevealChaosToken #when You AnyChaosToken

instance RunMessage ArkhamWoodsWoodenBridge where
  runMessage msg l@(ArkhamWoodsWoodenBridge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawAnotherChaosToken iid
      pure l
    _ -> ArkhamWoodsWoodenBridge <$> liftRunMessage msg attrs
