module Arkham.Asset.Cards.Divination1 (divination1, Divination1 (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Message.Discard (chooseAndDiscardCard)
import Arkham.Investigate
import Arkham.Message (getChoiceAmount)
import Arkham.Modifier

newtype Divination1 = Divination1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

divination1 :: AssetCard Divination1
divination1 = asset Divination1 Cards.divination1

instance HasAbilities Divination1 where
  getAbilities (Divination1 x) = [restrictedAbility x 1 ControlsThis investigateAction_]

instance RunMessage Divination1 where
  runMessage msg a@(Divination1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigate <- setTarget attrs <$> mkInvestigate iid (attrs.ability 1)
      skillTestModifier (attrs.ability 1) iid (AnySkillValue 1)
      chooseOne
        iid
        [ Label "Use {willpower} instead of {intellect}" [toMessage $ withSkillType #willpower investigate]
        , Label "Use {intellect}" [toMessage investigate]
        ]
      pure a
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) n -> do
      case attrs.use Charge of
        0 -> pure ()
        1 -> push $ ResolveAmounts iid [("Charges", 1)] (toTarget attrs)
        _ -> chooseAmounts iid "Amount of Charges to Spend" (MaxAmountTarget 2) [("Charges", (1, 2))] attrs
      pushWhen (n == 0) $ chooseAndDiscardCard iid (attrs.ability 1)
      pure a
    ResolveAmounts iid (getChoiceAmount "Charges" -> n) (isTarget attrs -> True) -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) n
      pure a
    _ -> Divination1 <$> liftRunMessage msg attrs
