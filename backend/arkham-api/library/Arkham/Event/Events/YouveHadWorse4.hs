module Arkham.Event.Events.YouveHadWorse4 (youveHadWorse4) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Healing
import Arkham.Helpers.Window (damagedInvestigator)
import Arkham.Investigator.Projection ()

newtype YouveHadWorse4 = YouveHadWorse4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youveHadWorse4 :: EventCard YouveHadWorse4
youveHadWorse4 = event YouveHadWorse4 Cards.youveHadWorse4

instance RunMessage YouveHadWorse4 where
  runMessage msg e@(YouveHadWorse4 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      let iid = damagedInvestigator attrs.windows
      resources <- iid.resources
      (damage, horror) <- getDamageAmounts iid
      let amounts = [("Damage", (0, damage)) | damage > 0] <> [("Horror", (0, horror)) | horror > 0]
      chooseAmounts
        iid
        "Amount of Damage/Horror to cancel"
        (MaxAmountTarget $ min 5 resources)
        amounts
        attrs
      pure e
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let damageAmount = getChoiceAmount "Damage" choices
      let horrorAmount = getChoiceAmount "Horror" choices
      moveTokens attrs iid attrs.owner #resource (damageAmount + horrorAmount)
      pushAll
        $ [CancelDamage iid damageAmount | damageAmount > 0]
        <> [CancelHorror iid horrorAmount | horrorAmount > 0]
      when (damageAmount + horrorAmount > 0) $ cancelledOrIgnoredCardOrGameEffect attrs
      pure e
    _ -> YouveHadWorse4 <$> liftRunMessage msg attrs
