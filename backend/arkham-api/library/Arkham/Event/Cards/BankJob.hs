module Arkham.Event.Cards.BankJob (bankJob, BankJob (..)) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype BankJob = BankJob EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bankJob :: EventCard BankJob
bankJob = event BankJob Cards.bankJob

instance RunMessage BankJob where
  runMessage msg e@(BankJob attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      iids <- select $ affectsOthers $ colocatedWith iid <> can.gain.resources
      replicateM_ 8 do
        chooseOrRunOne
          iid
          [ ResourceLabel iid' [TakeResources iid' 1 (toSource attrs) False]
          | iid' <- iids
          ]
      pure e
    _ -> BankJob <$> liftRunMessage msg attrs
