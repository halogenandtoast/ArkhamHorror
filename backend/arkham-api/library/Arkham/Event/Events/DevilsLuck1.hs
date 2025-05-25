module Arkham.Event.Events.DevilsLuck1 (devilsLuck1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (dealtDamage, dealtHorror)
import Arkham.Window qualified as Window

newtype DevilsLuck1 = DevilsLuck1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilsLuck1 :: EventCard DevilsLuck1
devilsLuck1 = event DevilsLuck1 Cards.devilsLuck1

instance RunMessage DevilsLuck1 where
  runMessage msg e@(DevilsLuck1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let (damage, horror) = (dealtDamage &&& dealtHorror) attrs.windows
      chooseAmounts
        iid
        "Amount of Damage/Horror to cancel"
        (MaxAmountTarget 10)
        ([("Damage", (0, damage)) | damage > 0] <> [("Horror", (0, horror)) | horror > 0])
        attrs
      pure e
    ResolveAmounts iid choices target | isTarget attrs target -> do
      let damageAmount = getChoiceAmount "Damage" choices
      when (damageAmount > 0) do
        push $ CancelDamage iid damageAmount

      let horrorAmount = getChoiceAmount "Horror" choices
      when (horrorAmount > 0) do
        push $ CancelHorror iid horrorAmount

      when (damageAmount + horrorAmount > 0) do
        checkAfter $ Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs
      pure e
    _ -> DevilsLuck1 <$> liftRunMessage msg attrs
