module Arkham.Asset.Cards.EarthlySerenity1 (earthlySerenity1, EarthlySerenity1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Helpers.Query (getPlayer)
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype EarthlySerenity1 = EarthlySerenity1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

earthlySerenity1 :: AssetCard EarthlySerenity1
earthlySerenity1 = asset EarthlySerenity1 Cards.earthlySerenity1

instance HasAbilities EarthlySerenity1 where
  getAbilities (EarthlySerenity1 a) = [restrictedAbility a 1 ControlsThis actionAbility]

instance RunMessage EarthlySerenity1 where
  runMessage msg a@(EarthlySerenity1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      beginSkillTest iid (attrs.ability 1) iid #willpower (Fixed 1)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      push $ DoStep n msg
      pushWhen (n == 0) $ LoseResources iid (attrs.ability 1) 1
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
          , Label "Spend a charge to heal" [SpendUses (toTarget attrs) Charge 1, Msg.chooseOne player choices]
          ]
      pure a
    _ -> EarthlySerenity1 <$> lift (runMessage msg attrs)
