module Arkham.Asset.Assets.LunarCharts (lunarCharts) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Dreaming (getDreamMoves)
import Arkham.Helpers.Dreaming.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype LunarCharts = LunarCharts AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lunarCharts :: AssetCard LunarCharts
lunarCharts = asset LunarCharts Cards.lunarCharts

instance HasAbilities LunarCharts where
  getAbilities (LunarCharts a) = [investigateAbility a 1 (assetUseCost a Secret 1) ControlsThis]

instance RunMessage LunarCharts where
  runMessage msg a@(LunarCharts attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
      investigate sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      moves <- getDreamMoves iid
      when (notNull moves) do
        chooseOneM iid do
          dreamMoveOptions iid (attrs.ability 1) moves
          cardI18n $ scope "lunarCharts" $ labeled "doNotMoveDream" nothing
      pure a
    _ -> LunarCharts <$> liftRunMessage msg attrs
