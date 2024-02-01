module Arkham.Location.Cards.EnchantedWoodsStoneTrapdoor (
  enchantedWoodsStoneTrapdoor,
  EnchantedWoodsStoneTrapdoor (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Projection

newtype EnchantedWoodsStoneTrapdoor = EnchantedWoodsStoneTrapdoor LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

enchantedWoodsStoneTrapdoor :: LocationCard EnchantedWoodsStoneTrapdoor
enchantedWoodsStoneTrapdoor = location EnchantedWoodsStoneTrapdoor Cards.enchantedWoodsStoneTrapdoor 2 (PerPlayer 1)

instance HasModifiersFor EnchantedWoodsStoneTrapdoor where
  getModifiersFor target (EnchantedWoodsStoneTrapdoor attrs) | attrs `is` target = do
    mSource <- getSkillTestSource
    mAction <- getSkillTestAction
    mInvestigator <- getSkillTestInvestigator
    case (mAction, mSource, mInvestigator) of
      (Just Action.Investigate, Just source, Just iid) | isSource attrs source -> do
        handSize <- fieldMap InvestigatorHand length iid
        pure $ toModifiers attrs [ShroudModifier handSize | locationRevealed attrs]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage EnchantedWoodsStoneTrapdoor where
  runMessage msg (EnchantedWoodsStoneTrapdoor attrs) =
    EnchantedWoodsStoneTrapdoor <$> runMessage msg attrs
