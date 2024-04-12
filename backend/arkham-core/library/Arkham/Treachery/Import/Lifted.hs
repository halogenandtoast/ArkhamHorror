module Arkham.Treachery.Import.Lifted (
  module X,
  module Arkham.Treachery.Import.Lifted,
)
where

import Arkham.Classes as X
import Arkham.Helpers.Query as X
import Arkham.Message as X (
  Message (..),
  toMessage,
  pattern AttachTreachery,
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
  pattern PassedThisSkillTest,
  pattern UseThisAbility,
 )
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Question as X
import Arkham.SkillTest.Base as X (SkillTestDifficulty (..))
import Arkham.Source as X
import Arkham.Target as X
import Arkham.Treachery.Runner as X (
  Field (..),
  IsTreachery,
  TreacheryAttrs,
  TreacheryCard,
  on,
  push,
  pushAll,
  pushM,
  pushWhen,
  setMeta,
  toModifiers,
  treachery,
  treacheryInHandOf,
  treacheryOn,
  treacheryOnEnemy,
  treacheryOnInvestigator,
  treacheryWith,
 )

import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Id
import Arkham.SkillType
import Arkham.Treachery.Helpers qualified as Msg

revelationSkillTest
  :: (Sourceable source, ReverseQueue m)
  => InvestigatorId
  -> source
  -> SkillType
  -> SkillTestDifficulty
  -> m ()
revelationSkillTest iid source sType n = push $ Msg.revelationSkillTest iid source sType n

attachTreachery
  :: (ReverseQueue m, AsId a, IdOf a ~ TreacheryId, Targetable target) => a -> target -> m ()
attachTreachery a target = push $ Msg.attachTreachery a target

gainSurge :: (ReverseQueue m, Sourceable a, Targetable a) => a -> m ()
gainSurge = push . Msg.gainSurge
