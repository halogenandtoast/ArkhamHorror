module Arkham.Location.Cards.TheDreamEaters.BeyondTheGatesOfSleep.EnchantedWoodsTheMoonTree (
  enchantedWoodsTheMoonTree,
  EnchantedWoodsTheMoonTree (..),
)
where

import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Scenarios.BeyondTheGatesOfSleep.Helpers

import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.CardDefs.TheDreamEaters.BeyondTheGatesOfSleep qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype EnchantedWoodsTheMoonTree = EnchantedWoodsTheMoonTree LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedWoodsTheMoonTree :: LocationCard EnchantedWoodsTheMoonTree
enchantedWoodsTheMoonTree = location EnchantedWoodsTheMoonTree Cards.enchantedWoodsTheMoonTree 3 (PerPlayer 1)

instance HasAbilities EnchantedWoodsTheMoonTree where
  getAbilities (EnchantedWoodsTheMoonTree a) =
    withRevealedAbilities a [forcedAbility a 1 $ Enters #after You $ be a]

instance RunMessage EnchantedWoodsTheMoonTree where
  runMessage msg l@(EnchantedWoodsTheMoonTree attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      remainingActions <- field InvestigatorRemainingActions iid
      chooseOrRunOneM iid do
        withI18n $ countVar 2 do
          labeled' "takeHorror" $ push $ assignHorror iid (toAbilitySource attrs 1) 2
        when (remainingActions > 0) do
          scenarioI18n $ scope "enchantedWoodsTheMoonTree" do
            labeled' "loseAllActions" $ push $ SetActions iid (toAbilitySource attrs 1) 0
      pure l
    _ -> EnchantedWoodsTheMoonTree <$> liftRunMessage msg attrs
