module Arkham.Asset.Cards.Divination4 (divination4, Divination4 (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Discard
import Arkham.Helpers.Message.Discard (discardFromHand)
import Arkham.Investigate
import Arkham.Message (getChoiceAmount)
import Arkham.Modifier

newtype Divination4 = Divination4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

divination4 :: AssetCard Divination4
divination4 = asset Divination4 Cards.divination4

instance HasAbilities Divination4 where
  getAbilities (Divination4 x) = [restrictedAbility x 1 ControlsThis investigateAction_]

instance RunMessage Divination4 where
  runMessage msg a@(Divination4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate <- setTarget attrs <$> mkInvestigate sid iid (attrs.ability 1)
      skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
      chooseOne
        iid
        [ Label "Use {willpower} instead of {intellect}" [toMessage $ withSkillType #willpower investigate]
        , Label "Use {intellect}" [toMessage investigate]
        ]
      pure a
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) n -> do
      case attrs.use Charge of
        0 -> pure ()
        1 -> do
          charges <- namedUUID "Charges"
          push $ ResolveAmounts iid [(charges, 1)] (toTarget attrs)
        _ -> chooseAmounts iid "Amount of Charges to Spend" (MaxAmountTarget 3) [("Charges", (1, 3))] attrs
      pushWhen (n == 0) $ discardFromHand iid (attrs.ability 1) DiscardChoose 2
      pure a
    ResolveAmounts iid (getChoiceAmount "Charges" -> n) (isTarget attrs -> True) -> do
      push $ SpendUses (attrs.ability 1) (toTarget attrs) Charge n
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) n
      pure a
    _ -> Divination4 <$> liftRunMessage msg attrs
