module Arkham.Asset.Assets.DialOfAncientsSignsOfRevelation4 (dialOfAncientsSignsOfRevelation4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosBag.RevealStrategy
import Arkham.Matcher
import Arkham.Modifier

newtype DialOfAncientsSignsOfRevelation4 = DialOfAncientsSignsOfRevelation4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dialOfAncientsSignsOfRevelation4 :: AssetCard DialOfAncientsSignsOfRevelation4
dialOfAncientsSignsOfRevelation4 = asset DialOfAncientsSignsOfRevelation4 Cards.dialOfAncientsSignsOfRevelation4

instance HasAbilities DialOfAncientsSignsOfRevelation4 where
  getAbilities (DialOfAncientsSignsOfRevelation4 a) =
    [restricted a 1 ControlsThis $ FastAbility' (exhaust a <> assetUseCost a Charge 1) [#investigate]]

instance RunMessage DialOfAncientsSignsOfRevelation4 where
  runMessage msg a@(DialOfAncientsSignsOfRevelation4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      x <- selectCount $ enemyAtLocationWith iid
      sid <- getRandom
      when (x > 0) do
        skillTestModifier sid (attrs.ability 1) sid (ChangeRevealStrategy $ RevealAndChoose (1 + x) 1)
      investigate sid iid (attrs.ability 1)
      pure a
    _ -> DialOfAncientsSignsOfRevelation4 <$> liftRunMessage msg attrs
