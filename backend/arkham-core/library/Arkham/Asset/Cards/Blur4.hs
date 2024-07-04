module Arkham.Asset.Cards.Blur4 (blur4, Blur4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Evade
import Arkham.Message (getChoiceAmount)
import Arkham.Modifier

newtype Blur4 = Blur4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blur4 :: AssetCard Blur4
blur4 = asset Blur4 Cards.blur4

instance HasAbilities Blur4 where
  getAbilities (Blur4 x) = [controlledAbility x 1 blurCriteria evadeAction_]
   where
    blurCriteria = if hasUses x then NoRestriction else Never

instance RunMessage Blur4 where
  runMessage msg a@(Blur4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (AnySkillValue 2)
      evade <- mkChooseEvade iid (attrs.ability 1)
      chooseOne
        iid
        [ Label "Use willpower" [toMessage $ withSkillType #willpower evade]
        , Label "Use agility" [toMessage evade]
        ]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      case attrs.use Charge of
        0 -> pure ()
        1 -> push $ ResolveAmounts iid [("Charges", 1)] (toTarget attrs)
        (min 2 -> x) ->
          chooseAmounts iid "Amount of Charges to Spend" (MaxAmountTarget x) [("Charges", (1, x))] attrs
      when (n == 0) $ assignDamage iid (attrs.ability 1) 2
      pure a
    ResolveAmounts iid (getChoiceAmount "Charges" -> n) (isTarget attrs -> True) -> do
      push $ SpendUses (toTarget attrs) Charge n
      gainActions iid (attrs.ability 1) n
      pure a
    _ -> Blur4 <$> liftRunMessage msg attrs
