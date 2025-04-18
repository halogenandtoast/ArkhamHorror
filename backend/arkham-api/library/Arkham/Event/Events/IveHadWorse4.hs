module Arkham.Event.Events.IveHadWorse4 (iveHadWorse4) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Healing

newtype IveHadWorse4 = IveHadWorse4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveHadWorse4 :: EventCard IveHadWorse4
iveHadWorse4 = event IveHadWorse4 Cards.iveHadWorse4

instance RunMessage IveHadWorse4 where
  runMessage msg e@(IveHadWorse4 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      (damage, horror) <- getDamageAmounts iid
      let amounts = [("Damage", (0, damage)) | damage > 0] <> [("Horror", (0, horror)) | horror > 0]
      chooseAmounts iid "Amount of Damage/Horror to cancel" (MaxAmountTarget 5) amounts attrs
      pure e
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let damageAmount = getChoiceAmount "Damage" choices
      let horrorAmount = getChoiceAmount "Horror" choices
      pushAll
        $ [CancelDamage iid damageAmount | damageAmount > 0]
        <> [CancelHorror iid horrorAmount | horrorAmount > 0]
      gainResourcesIfCan iid attrs (damageAmount + horrorAmount)
      when (damageAmount + horrorAmount > 0) $ cancelledOrIgnoredCardOrGameEffect attrs
      pure e
    _ -> IveHadWorse4 <$> liftRunMessage msg attrs
