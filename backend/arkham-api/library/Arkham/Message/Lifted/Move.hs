module Arkham.Message.Lifted.Move where

import Arkham.Card.CardDef
import Arkham.Classes.HasQueue (push)
import Arkham.Id
import Arkham.Matcher.Location
import Arkham.Message hiding (story)
import Arkham.Message.Lifted.Queue
import Arkham.Movement
import Arkham.Movement qualified as Msg
import Arkham.Prelude
import Arkham.Source
import Arkham.Target

moveAllTo :: (ReverseQueue m, Sourceable source) => source -> LocationId -> m ()
moveAllTo (toSource -> source) lid = push $ MoveAllTo source lid

moveTo
  :: (ReverseQueue m, Sourceable source, AsId location, IdOf location ~ LocationId)
  => source -> InvestigatorId -> location -> m ()
moveTo (toSource -> source) iid location = push $ Move $ move source iid (asId location)

moveToMatch :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> LocationMatcher -> m ()
moveToMatch (toSource -> source) iid = push . Move . Arkham.Movement.moveToMatch source iid

enemyMoveTo
  :: (ReverseQueue m, AsId enemy, IdOf enemy ~ EnemyId, AsId location, IdOf location ~ LocationId)
  => enemy
  -> location
  -> m ()
enemyMoveTo enemy location = push $ EnemyMove (asId enemy) (asId location)

moveUntil
  :: (ReverseQueue m, Targetable target, AsId location, IdOf location ~ LocationId)
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
  asMoveTo :: (Sourceable source, Targetable target) => source -> target -> a -> Movement

data MoveWrapper where
  CannotCancel :: AsMoveTo a => a -> MoveWrapper

instance AsMoveTo Movement where
  asMoveTo _ _ = id

instance AsMoveTo LocationId where
  asMoveTo = move

instance AsMoveTo LocationMatcher where
  asMoveTo = Arkham.Movement.moveToMatch

instance AsMoveTo CardDef where
  asMoveTo source target = Arkham.Movement.moveToMatch source target . locationIs

instance AsMoveTo MoveWrapper where
  asMoveTo source iid = \case
    CannotCancel inner -> uncancellableMove (asMoveTo source iid inner)

-- No callbacks
moveTo_
  :: (ReverseQueue m, Sourceable source, AsMoveTo movement, Targetable target)
  => source
  -> target
  -> movement
  -> m ()
moveTo_ (toSource -> source) target = push . MoveTo . asMoveTo source target

moveTowardsMatching
  :: (Targetable target, Sourceable source, ReverseQueue m)
  => source
  -> target
  -> LocationMatcher
  -> m ()
moveTowardsMatching source target matcher = push $ Move $ Msg.moveTowardsMatching source target matcher

moveTowards
  :: (Targetable target, Sourceable source, ReverseQueue m, AsId location, IdOf location ~ LocationId)
  => source
  -> target
  -> location
  -> m ()
moveTowards source target location = push $ Move $ Msg.moveTowardsMatching source target (LocationWithId $ asId location)
