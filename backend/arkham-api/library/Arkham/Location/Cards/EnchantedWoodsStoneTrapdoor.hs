module Arkham.Location.Cards.EnchantedWoodsStoneTrapdoor (
  enchantedWoodsStoneTrapdoor,
  EnchantedWoodsStoneTrapdoor (..),
)
where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getIsBeingInvestigated, getSkillTestInvestigator)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Projection

newtype EnchantedWoodsStoneTrapdoor = EnchantedWoodsStoneTrapdoor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

enchantedWoodsStoneTrapdoor :: LocationCard EnchantedWoodsStoneTrapdoor
enchantedWoodsStoneTrapdoor = location EnchantedWoodsStoneTrapdoor Cards.enchantedWoodsStoneTrapdoor 2 (PerPlayer 1)

instance HasModifiersFor EnchantedWoodsStoneTrapdoor where
  getModifiersFor (EnchantedWoodsStoneTrapdoor attrs) = modifySelfMaybe attrs do
    liftGuardM $ getIsBeingInvestigated attrs.id
    iid <- MaybeT getSkillTestInvestigator
    handSize <- lift $ fieldMap InvestigatorHand length iid
    pure [ShroudModifier handSize]

instance RunMessage EnchantedWoodsStoneTrapdoor where
  runMessage msg (EnchantedWoodsStoneTrapdoor attrs) =
    EnchantedWoodsStoneTrapdoor <$> runMessage msg attrs
