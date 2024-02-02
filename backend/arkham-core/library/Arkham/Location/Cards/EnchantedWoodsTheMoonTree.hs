module Arkham.Location.Cards.EnchantedWoodsTheMoonTree (
  enchantedWoodsTheMoonTree,
  EnchantedWoodsTheMoonTree (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype EnchantedWoodsTheMoonTree = EnchantedWoodsTheMoonTree LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

enchantedWoodsTheMoonTree :: LocationCard EnchantedWoodsTheMoonTree
enchantedWoodsTheMoonTree = location EnchantedWoodsTheMoonTree Cards.enchantedWoodsTheMoonTree 3 (PerPlayer 1)

instance HasAbilities EnchantedWoodsTheMoonTree where
  getAbilities (EnchantedWoodsTheMoonTree a) =
    withRevealedAbilities a [forcedAbility a 1 $ Enters #after You $ be a]

instance RunMessage EnchantedWoodsTheMoonTree where
  runMessage msg l@(EnchantedWoodsTheMoonTree attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      remainingActions <- field InvestigatorRemainingActions iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [Label "Take 2 horror" [assignHorror iid (toAbilitySource attrs 1) 2]]
        <> [ Label "Lose all remaining actions" [SetActions iid (toAbilitySource attrs 1) 0]
           | remainingActions > 0
           ]
      pure l
    _ -> EnchantedWoodsTheMoonTree <$> runMessage msg attrs
