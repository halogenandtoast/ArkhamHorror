module Arkham.Asset.Cards.EarthlySerenity4 (earthlySerenity4, EarthlySerenity4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Helpers.Query (getPlayer)
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype EarthlySerenity4 = EarthlySerenity4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

earthlySerenity4 :: AssetCard EarthlySerenity4
earthlySerenity4 = asset EarthlySerenity4 Cards.earthlySerenity4

instance HasAbilities EarthlySerenity4 where
  getAbilities (EarthlySerenity4 a) = [skillTestAbility $ restrictedAbility a 1 ControlsThis actionAbility]

instance RunMessage EarthlySerenity4 where
  runMessage msg a@(EarthlySerenity4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 0)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      push $ DoStep n msg
      pushWhen (n == 0) $ LoseResources iid (attrs.ability 1) 2
      pure a
    DoStep n (PassedThisSkillTest iid (isAbilitySource attrs 1 -> True)) | n > 0 -> do
      targets <- select $ affectsOthers $ colocatedWith iid
      choices <- concatForM targets \target -> do
        canHealDamage <- canHaveDamageHealed (attrs.ability 1) target
        canHealHorror <- canHaveHorrorHealed (attrs.ability 1) target
        pure
          $ [DamageLabel target [HealDamage (toTarget target) (attrs.ability 1) 1] | canHealDamage]
          <> [HorrorLabel target [HealHorror (toTarget target) (attrs.ability 1) 1] | canHealHorror]
      when (notNull choices && hasUses attrs) do
        player <- getPlayer iid
        chooseOne
          iid
          [ Label "Do not spend a charge to heal" []
          , Label
              "Spend a charge to heal"
              [SpendUses (attrs.ability 1) (toTarget attrs) Charge 1, Msg.chooseOne player choices]
          ]
      pure a
    _ -> EarthlySerenity4 <$> liftRunMessage msg attrs
