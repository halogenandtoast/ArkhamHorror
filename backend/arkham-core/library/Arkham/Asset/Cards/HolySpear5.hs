module Arkham.Asset.Cards.HolySpear5 (holySpear5, HolySpear5 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Modifier

newtype HolySpear5 = HolySpear5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

holySpear5 :: AssetCard HolySpear5
holySpear5 = asset HolySpear5 Cards.holySpear5

instance HasAbilities HolySpear5 where
  getAbilities (HolySpear5 attrs) =
    [ withTooltip
        "{action}: _Fight_. You get +2 {willpower} and deal +1 damage for this attack. When you initiate this ability, you may release a {bless} token sealed on Holy Spear."
        $ restrictedAbility attrs 1 ControlsThis fightAction_
    , withTooltip
        "{action} Search the chaos bag for 2 {bless} tokens and seal them on Holy Spear: _Fight_. You get +4{combat} and deal +2 damage for this attack."
        $ restrictedAbility attrs 2 ControlsThis
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
          [ Label "Release a {bless} token" [UnsealChaosToken blessToken]
          , Label "Do not release a {bless} token" []
          ]
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 2, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 4, DamageDealt 2]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> HolySpear5 <$> liftRunMessage msg attrs
