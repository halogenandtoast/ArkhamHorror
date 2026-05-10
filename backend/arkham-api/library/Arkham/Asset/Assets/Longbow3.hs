module Arkham.Asset.Assets.Longbow3 (longbow3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype Longbow3 = Longbow3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

longbow3 :: AssetCard Longbow3
longbow3 = asset Longbow3 Cards.longbow3

instance HasAbilities Longbow3 where
  getAbilities (Longbow3 a) =
    [ skillTestAbility $ controlled_ a 1 $ fightActionWith #agility $ assetUseCost a Ammo 1
    , (cardI18n $ withI18nTooltip "longbow3.youNockAnotherArrow") $ controlled a 2 (thisExists a AssetNotAtUsesX) actionAbility
    ]

instance RunMessage Longbow3 where
  runMessage msg a@(Longbow3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #agility 2, DamageDealt 2, IgnoreAloof]
      chooseFightEnemyWith #agility sid iid (attrs.ability 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      addUses (attrs.ability 2) attrs Ammo 1
      pure a
    _ -> Longbow3 <$> liftRunMessage msg attrs
