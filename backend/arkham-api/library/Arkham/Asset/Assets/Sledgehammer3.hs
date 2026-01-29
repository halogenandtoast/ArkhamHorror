module Arkham.Asset.Assets.Sledgehammer3 (sledgehammer3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Sledgehammer3 = Sledgehammer3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sledgehammer3 :: AssetCard Sledgehammer3
sledgehammer3 = asset Sledgehammer3 Cards.sledgehammer3

instance HasAbilities Sledgehammer3 where
  getAbilities (Sledgehammer3 a) =
    [ skillTestAbility $ controlled_ a 1 fightAction_
    , skillTestAbility $ controlled_ a 2 $ ActionAbility [#fight] #combat (ActionCost 2)
    ]

instance RunMessage Sledgehammer3 where
  runMessage msg a@(Sledgehammer3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat 3)
      chooseFightEnemy sid iid (attrs.ability 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) | attrs.ready -> do
      withSkillTest \sid -> do
        chooseOneM iid do
          labeled "Exhaust Sledgehammer" do
            exhaustThis attrs
            skillTestModifier sid (attrs.ability 2) iid (DamageDealt 2)
          withI18n skip_
      pure a
    _ -> Sledgehammer3 <$> liftRunMessage msg attrs
