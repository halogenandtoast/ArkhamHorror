module Arkham.Event.Events.YouveHadWorse (youveHadWorse) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Healing
import Arkham.Helpers.Window (damagedInvestigator)
import Arkham.Investigator.Projection ()

newtype YouveHadWorse = YouveHadWorse EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youveHadWorse :: EventCard YouveHadWorse
youveHadWorse = event YouveHadWorse Cards.youveHadWorse

instance RunMessage YouveHadWorse where
  runMessage msg e@(YouveHadWorse attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      let iid = damagedInvestigator attrs.windows
      resources <- iid.resources
      (damage, horror) <- getDamageAmounts iid
      let amounts = [("Damage", (0, damage)) | damage > 0] <> [("Horror", (0, horror)) | horror > 0]
      chooseAmounts
        iid
        "Amount of Damage/Horror to cancel"
        (MaxAmountTarget $ min 3 resources)
        amounts
        attrs
      pure e
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let damageAmount = getChoiceAmount "Damage" choices
      let horrorAmount = getChoiceAmount "Horror" choices
      pushAll
        $ SpendResources iid (damageAmount + horrorAmount)
        : [CancelDamage iid damageAmount | damageAmount > 0]
          <> [CancelHorror iid horrorAmount | horrorAmount > 0]
      when (damageAmount + horrorAmount > 0) $ cancelledOrIgnoredCardOrGameEffect attrs
      pure e
    _ -> YouveHadWorse <$> liftRunMessage msg attrs
