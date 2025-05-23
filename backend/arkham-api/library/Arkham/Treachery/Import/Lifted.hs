module Arkham.Treachery.Import.Lifted (
  module X,
  module Arkham.Treachery.Import.Lifted,
)
where

import Arkham.Calculation as X
import Arkham.Classes as X
import Arkham.Helpers.Choices as X
import Arkham.Helpers.Query as X
import Arkham.Id as X
import Arkham.Message as X (
  Message (..),
  ShuffleIn (..),
  toMessage,
  pattern FailedThisSkillTest,
  pattern FailedThisSkillTestBy,
  pattern PassedThisSkillTest,
  pattern PassedThisSkillTestBy,
  pattern PlaceDoom,
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
  canBeCommittedL,
  forcedOnElimination,
  metaL,
  on,
  push,
  pushAll,
  pushM,
  pushWhen,
  setMeta,
  tokensL,
  treachery,
  treacheryHorror,
  treacheryInHandOf,
  treacheryInThreatArea,
  treacheryOn,
  treacheryOnAgenda,
  treacheryOnEnemy,
  treacheryOnLocation,
  treacheryOnTopOfDeck,
  treacheryWith,
  waitingL,
  withTreacheryInvestigator,
  pattern PlaceResources,
 )

import Arkham.Card.CardCode
import Arkham.Classes.HasGame
import Arkham.Helpers.History
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Message.Lifted.Placement
import Arkham.SkillType
import Arkham.Treachery.Helpers qualified as Msg

revelationSkillTest
  :: (Sourceable source, ReverseQueue m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> SkillType
  -> GameCalculation
  -> m ()
revelationSkillTest sid iid source sType n = push $ Msg.revelationSkillTest sid iid source sType n

attachTreachery
  :: ( ReverseQueue m
     , AsId a
     , IdOf a ~ TreacheryId
     , Targetable target
     , Show target
     , Msg.NotEqual target InvestigatorId
     )
  => a
  -> target
  -> m ()
attachTreachery a target = push $ Msg.attachTreachery a target

placeInThreatArea
  :: (ReverseQueue m, AsId a, IdOf a ~ TreacheryId)
  => a
  -> InvestigatorId
  -> m ()
placeInThreatArea t = push . Msg.placeInThreatArea t

placeTreachery
  :: (ReverseQueue m, AsId a, IdOf a ~ TreacheryId)
  => a
  -> Placement
  -> m ()
placeTreachery t = push . Msg.PlaceTreachery (asId t)

gainSurge :: (ReverseQueue m, Sourceable a, Targetable a) => a -> m ()
gainSurge = push . Msg.gainSurge

guardInThreatArea :: Monad m => InvestigatorId -> TreacheryAttrs -> MaybeT m ()
guardInThreatArea iid attrs = guard $ treacheryInThreatArea iid attrs

isFirstCopyThisPhase :: (HasGame m, HasCardCode a) => a -> m Bool
isFirstCopyThisPhase attrs = do
  drawn <- getAllHistoryField #phase HistoryTreacheriesDrawn
  pure $ count (== toCardCode attrs) drawn == 1

addHiddenToHand :: ReverseQueue m => InvestigatorId -> TreacheryAttrs -> m ()
addHiddenToHand iid a = place a (HiddenInHand iid)
