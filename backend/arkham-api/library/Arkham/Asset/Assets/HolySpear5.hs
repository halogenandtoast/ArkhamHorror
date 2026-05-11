module Arkham.Asset.Assets.HolySpear5 (holySpear5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Modifier

newtype HolySpear5 = HolySpear5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holySpear5 :: AssetCard HolySpear5
holySpear5 = asset HolySpear5 Cards.holySpear5

instance HasAbilities HolySpear5 where
  getAbilities (HolySpear5 attrs) =
    [ cardI18n
        $ scope "holySpear5"
        $ withI18nTooltip "fightWithReleaseBless"
        $ controlled_ attrs 1 fightAction_
    , cardI18n
        $ scope "holySpear5"
        $ withI18nTooltip "fightWithSealBless"
        $ controlled_ attrs 2
        $ fightAction
        $ SealMultiCost 2 #bless
    ]

instance RunMessage HolySpear5 where
  runMessage msg a@(HolySpear5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let mBlessToken = find ((== #bless) . (.face)) attrs.sealedChaosTokens
      for_ mBlessToken $ \blessToken -> do
        chooseOne
          iid
          [ Label "$cards.label.holySpear5.releaseBless" [UnsealChaosToken blessToken]
          , Label "$cards.label.holySpear5.doNotReleaseBless" []
          ]
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 2, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 2) iid [SkillModifier #combat 4, DamageDealt 2]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> HolySpear5 <$> liftRunMessage msg attrs
