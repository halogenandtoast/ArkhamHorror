module Arkham.Message.Lifted.Log (module Arkham.Message.Lifted.Log, module X) where

import Arkham.Helpers.Log as X hiding (crossOutRecordSetEntries, recordSetInsert, recordSetReplace)

import Arkham.CampaignLogKey
import Arkham.Classes.HasQueue
import Arkham.Helpers.Log qualified as Msg
import Arkham.Id
import Arkham.Message (Message (..))
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.ScenarioLogKey

record :: (ReverseQueue m, IsCampaignLogKey k) => k -> m ()
record = push . Record . toCampaignLogKey

recordWhen :: (ReverseQueue m, IsCampaignLogKey k) => Bool -> k -> m ()
recordWhen True = push . Record . toCampaignLogKey
recordWhen False = pure . const ()

recordCount :: (ReverseQueue m, IsCampaignLogKey k) => k -> Int -> m ()
recordCount k = push . RecordCount (toCampaignLogKey k)

remember :: ReverseQueue m => ScenarioLogKey -> m ()
remember = push . Remember

forget :: ReverseQueue m => ScenarioLogKey -> m ()
forget = push . Forget

recordForInvestigator
  :: (IsCampaignLogKey k, ReverseQueue m, AsId investigator, IdOf investigator ~ InvestigatorId)
  => investigator -> k -> m ()
recordForInvestigator iid k = push $ RecordForInvestigator (asId iid) (toCampaignLogKey k)

crossOut :: (ReverseQueue m, IsCampaignLogKey k) => k -> m ()
crossOut = push . CrossOutRecord . toCampaignLogKey

crossOutWhen :: (ReverseQueue m, IsCampaignLogKey k) => Bool -> k -> m ()
crossOutWhen cond k = when cond $ crossOut k

recordSetInsert
  :: (Recordable a, MonoFoldable t, Element t ~ a, ReverseQueue m, IsCampaignLogKey k)
  => k
  -> t
  -> m ()
recordSetInsert k = push . Msg.recordSetInsert k

recordSetReplace
  :: (ReverseQueue m, IsCampaignLogKey k) => k -> SomeRecorded -> SomeRecorded -> m ()
recordSetReplace k v v' = push $ Msg.recordSetReplace (toCampaignLogKey k) v v'

incrementRecordCount :: (ReverseQueue m, IsCampaignLogKey k) => k -> Int -> m ()
incrementRecordCount key = push . IncrementRecordCount (toCampaignLogKey key)

crossOutRecordSetEntries :: (Recordable a, ReverseQueue m, IsCampaignLogKey k) => k -> [a] -> m ()
crossOutRecordSetEntries _ [] = pure ()
crossOutRecordSetEntries k xs = push $ Msg.crossOutRecordSetEntries k xs
