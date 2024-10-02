module Arkham.Event.Events.IveHadWorse2 (iveHadWorse2, IveHadWorse2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Message (MessageType (..), messageType)

newtype IveHadWorse2 = IveHadWorse2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveHadWorse2 :: EventCard IveHadWorse2
iveHadWorse2 = event IveHadWorse2 Cards.iveHadWorse2

dropUntilDamage :: [Message] -> [Message]
dropUntilDamage = dropWhile (notElem DamageMessage . messageType)

instance RunMessage IveHadWorse2 where
  runMessage msg e@(IveHadWorse2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      (damage, horror) <- fromQueue $ \queue -> case dropUntilDamage queue of
        dmsg : _ ->
          case dmsg of
            InvestigatorDamage iid' _ damage' horror' ->
              if iid' == iid then (damage', horror') else error "mismatch"
            InvestigatorDoAssignDamage iid' _ _ _ damage' horror' _ _ ->
              if iid' == iid then (damage', horror') else error "mismatch"
            _ -> error "mismatch"
        _ -> error "unhandled"
      let amounts = [("Damage", (0, damage)) | damage > 0] <> [("Horror", (0, horror)) | horror > 0]
      chooseAmounts iid "Amount of Damage/Horror to cancel" (MaxAmountTarget 2) amounts attrs
      pure e
    ResolveAmounts iid choices target | isTarget attrs target -> do
      let
        damageAmount = getChoiceAmount "Damage" choices
        horrorAmount = getChoiceAmount "Horror" choices
      pushAll
        $ [CancelDamage iid damageAmount | damageAmount > 0]
        <> [CancelHorror iid horrorAmount | horrorAmount > 0]
      gainResourcesIfCan iid attrs (damageAmount + horrorAmount)
      when (damageAmount + horrorAmount > 0) $ cancelledOrIgnoredCardOrGameEffect attrs
      pure e
    _ -> IveHadWorse2 <$> liftRunMessage msg attrs
