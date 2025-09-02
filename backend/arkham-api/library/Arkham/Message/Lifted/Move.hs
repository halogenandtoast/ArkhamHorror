module Arkham.Message.Lifted.Move where

import Arkham.Card.CardDef
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query (whenMatch)
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher.Enemy
import Arkham.Matcher.Location
import Arkham.Message hiding (story)
import Arkham.Message.Lifted.Queue
import Arkham.Movement
import Arkham.Movement qualified as Msg
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Target

moveAllTo :: (ReverseQueue m, Sourceable source) => source -> LocationId -> m ()
moveAllTo (toSource -> source) lid = push $ MoveAllTo source lid

moveTo
  :: (ReverseQueue m, Sourceable source, ToId location LocationId)
  => source -> InvestigatorId -> location -> m ()
moveTo source iid location = moveToEdit source iid location id

moveToEdit
  :: (ReverseQueue m, Sourceable source, ToId location LocationId)
  => source -> InvestigatorId -> location -> (Movement -> Movement) -> m ()
moveToEdit (toSource -> source) iid location f = do
  field InvestigatorPlacement (asId iid) >>= \case
    AtLocation current -> when (current /= asId location) do
      push . Move . f =<< move source iid (asId location)
    _ -> push . Move . f =<< move source iid (asId location)

moveToMatch
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> LocationMatcher -> m ()
moveToMatch (toSource -> source) iid = push . Move <=< Arkham.Movement.moveToMatch source iid

enemyMoveTo
  :: (ReverseQueue m, Sourceable source, Targetable enemy, ToId location LocationId)
  => source
  -> enemy
  -> location
  -> m ()
enemyMoveTo source enemy location = push . Move =<< asMoveTo source enemy (asId location)

enemyMoveToEdit
  :: ( ReverseQueue m
     , Sourceable source
     , Targetable enemy
     , ToId enemy EnemyId
     , ToId location LocationId
     )
  => source
  -> enemy
  -> location
  -> (Movement -> Movement)
  -> m ()
enemyMoveToEdit source enemy location f = do
  whenMatch (asId enemy) EnemyCanMove do
    push . Move . f =<< asMoveTo source enemy (asId location)

enemyMoveToMatch
  :: ( ReverseQueue m
     , Targetable enemy
     , IsLocationMatcher matcher
     , Sourceable source
     )
  => source
  -> enemy
  -> matcher
  -> m ()
enemyMoveToMatch source enemy = push . Move <=< Arkham.Movement.moveToMatch source enemy . toLocationMatcher

moveUntil
  :: (ReverseQueue m, Targetable target, ToId location LocationId)
  => target
  -> location
  -> m ()
moveUntil target location = push $ MoveUntil (asId location) (toTarget target)

moveToward
  :: (ReverseQueue m, Targetable target, IsLocationMatcher matcher)
  => target
  -> matcher
  -> m ()
moveToward target matcher = push $ MoveToward (toTarget target) (toLocationMatcher matcher)

class AsMoveTo a where
  asMoveTo
    :: (MonadRandom m, Sourceable source, Targetable target) => source -> target -> a -> m Movement

data MoveWrapper where
  CannotCancel :: AsMoveTo a => a -> MoveWrapper

instance AsMoveTo Movement where
  asMoveTo _ _ = pure

instance AsMoveTo LocationId where
  asMoveTo = move

instance AsMoveTo LocationMatcher where
  asMoveTo = Arkham.Movement.moveToMatch

instance AsMoveTo CardDef where
  asMoveTo source target = Arkham.Movement.moveToMatch source target . locationIs

instance AsMoveTo MoveWrapper where
  asMoveTo source iid = \case
    CannotCancel inner -> uncancellableMove <$> asMoveTo source iid inner

-- No callbacks
moveTo_
  :: (ReverseQueue m, Sourceable source, AsMoveTo movement, Targetable target)
  => source
  -> target
  -> movement
  -> m ()
moveTo_ (toSource -> source) target = push . MoveTo <=< asMoveTo source target

moveTowardsMatching
  :: (Targetable target, Sourceable source, ReverseQueue m)
  => source
  -> target
  -> LocationMatcher
  -> m ()
moveTowardsMatching source target matcher = push . Move =<< Msg.moveTowardsMatching source target matcher

moveTowards
  :: (Targetable target, Sourceable source, ReverseQueue m, ToId location LocationId)
  => source
  -> target
  -> location
  -> m ()
moveTowards source target location = push . Move =<< Msg.moveTowardsMatching source target (LocationWithId $ asId location)
