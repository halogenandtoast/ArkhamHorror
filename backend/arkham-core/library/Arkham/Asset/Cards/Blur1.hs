module Arkham.Asset.Cards.Blur1 (blur1, Blur1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Evade
import Arkham.Modifier

newtype Blur1 = Blur1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blur1 :: AssetCard Blur1
blur1 = asset Blur1 Cards.blur1

instance HasAbilities Blur1 where
  getAbilities (Blur1 x) = [controlledAbility x 1 blurCriteria evadeAction_]
   where
    blurCriteria = if hasUses x then NoRestriction else Never

instance RunMessage Blur1 where
  runMessage msg a@(Blur1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (AnySkillValue 1)
      evade <- mkChooseEvade iid (attrs.ability 1)
      chooseOne
        iid
        [ Label "Use willpower" [toMessage $ withSkillType #willpower evade]
        , Label "Use agility" [toMessage evade]
        ]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      push $ SpendUses (toTarget attrs) Charge 1
      gainActions iid (attrs.ability 1) 1
      when (n == 0) $ assignDamage iid (attrs.ability 1) 1
      pure a
    _ -> Blur1 <$> lift (runMessage msg attrs)
