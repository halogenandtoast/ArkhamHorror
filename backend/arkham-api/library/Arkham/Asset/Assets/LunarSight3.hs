module Arkham.Asset.Assets.LunarSight3 (lunarSight3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Dreaming.Lifted (dreamOrElse)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest.Lifted (investigateWith_)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype LunarSight3 = LunarSight3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lunarSight3 :: AssetCard LunarSight3
lunarSight3 = asset LunarSight3 Cards.lunarSight3

instance HasAbilities LunarSight3 where
  getAbilities (LunarSight3 a) =
    [ skillTestAbility
        $ controlled a 1 (exists $ YourLocation <> InvestigatableLocation)
        $ investigateActionWith_ #willpower
    ]

instance RunMessage LunarSight3 where
  runMessage msg a@(LunarSight3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #skull attrs attrs $ doStep 1 msg
      whenM (attrs.id <=~> AssetWithSpendableUses (atLeast 1) Charge) do
        chooseOneM iid $ cardI18n $ scope "lunarSight3" do
          labeled "spendChargeForShroudAndClue" do
            spendUses (attrs.ability 1) attrs Charge 1
            withLocationOf iid \lid -> skillTestModifier sid (attrs.ability 1) lid (ShroudModifier (-2))
            skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
          withI18n skip_
      investigateWith_ #willpower sid iid (attrs.ability 1)
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      cardI18n $ scope "lunarSight3" $ dreamOrElse iid (attrs.ability 1) "takeHorrorAndDiscard" do
        assignHorror iid (attrs.ability 1) 1
        toDiscardBy iid (attrs.ability 1) attrs
      pure a
    _ -> LunarSight3 <$> liftRunMessage msg attrs
