module Arkham.Asset.Cards.HallowedChalice (hallowedChalice, HallowedChalice (..)) where

import Arkham.Ability.Builder
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorDamage)
import Arkham.Damage
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (WindowType (..), windowType)

newtype HallowedChalice = HallowedChalice AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallowedChalice :: AssetCard HallowedChalice
hallowedChalice = asset HallowedChalice Cards.hallowedChalice

instance HasAbilities HallowedChalice where
  getAbilities (HallowedChalice a) = abilities a $ withActionAbility 1 do
    mustControl
    mustExist
      $ oneOf
        [HealableInvestigator (a.ability 1) kind (InvestigatorAt YourLocation) | kind <- [#damage, #horror]]

instance RunMessage HallowedChalice where
  runMessage msg a@(HallowedChalice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      selectWithNonNull
        (oneOf [HealableInvestigator source kind (colocatedWith iid) | kind <- [#damage, #horror]])
        $ chooseOneToHandle iid source
      pure $ overAttrs (unsetMetaKey "option2") a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      horrorHealable <- canHaveHorrorHealed (attrs.ability 1) iid'
      damageHealable <- canHaveDamageHealed (attrs.ability 1) iid'

      chooseOneM iid do
        when attrs.ready do
          labeled
            "Exhaust Hallowed Chalice and place 1 doom on it to heal 2 damage or 2 horror from that investigator."
            do
              push $ Exhaust (toTarget attrs)
              placeDoom (attrs.ability 1) attrs 1
              chooseOrRunOne
                iid
                $ [Label "Heal 2 damage" [HealDamage (toTarget iid') (attrs.ability 1) 2] | damageHealable]
                <> [Label "Heal 2 horror" [HealHorror (toTarget iid') (attrs.ability 1) 2] | horrorHealable]
        labeled
          "Heal 1 damage or 1 horror from that investigator. If you heal the last damage or horror from that investigator, remove 1 doom from Hallowed Chalice."
          do
            doStep 1 msg
            chooseOrRunOne
              iid
              $ [Label "Heal 1 damage" [HealDamage (toTarget iid') (attrs.ability 1) 1] | damageHealable]
              <> [Label "Heal 1 horror" [HealHorror (toTarget iid') (attrs.ability 1) 1] | horrorHealable]
      pure a
    DoStep 1 (HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) _) -> do
      pure $ overAttrs (setMetaKey "option2" True) a
    RunWindow iid windows | attrs `controlledBy` iid && getMetaKey "option2" attrs -> do
      healedLast <- flip anyM (map windowType windows) \case
        Healed DamageType (InvestigatorTarget iid') (isAbilitySource attrs 1 -> True) _ -> not <$> fieldSome InvestigatorDamage iid'
        Healed HorrorType (InvestigatorTarget iid') (isAbilitySource attrs 1 -> True) _ -> not <$> fieldSome InvestigatorHorror iid'
        _ -> pure False
      if healedLast && attrs.doom > 0
        then do
          removeDoom (attrs.ability 1) (toTarget attrs) 1
          pure $ overAttrs (unsetMetaKey "option2") a
        else pure a
    _ -> HallowedChalice <$> liftRunMessage msg attrs
