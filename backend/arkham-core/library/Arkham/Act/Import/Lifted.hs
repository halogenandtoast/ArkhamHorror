module Arkham.Act.Import.Lifted (
  module X,
  module Arkham.Act.Import.Lifted,
)
where

import Arkham.Act.Helpers as X (groupClueCost)
import Arkham.Act.Runner as X (
  ActAttrs (..),
  ActCard,
  ActSide (..),
  AdvancementMethod (..),
  IsAct,
  Message (..),
  act,
  getLeadPlayer,
  getSetAsideCard,
  isSide,
  onSide,
  push,
  pushAll,
  targetLabel,
  whenHasRecord,
  pattern R1,
  pattern R2,
  pattern R3,
  pattern R4,
  pattern R5,
  pattern R6,
  pattern R7,
  pattern R8,
  pattern UseThisAbility,
 )
import Arkham.Classes as X
import Arkham.Cost as X
import Arkham.GameValue as X
import Arkham.Helpers.Log as X (getHasRecord)
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Source as X
import Arkham.Target as X
import Arkham.Text as X

import Arkham.Helpers.Act qualified as Msg
import Arkham.Id

advanceVia
  :: (ReverseQueue m, EntityId a ~ ActId, Sourceable source, Entity a)
  => AdvancementMethod
  -> a
  -> source
  -> m ()
advanceVia method actId source = push $ Msg.advanceVia method actId source
