module Base.Lock where

import ClassyPrelude
import Lens.Micro

class HasLock a where
  type LockKey a
  lock :: Lens' a (Maybe (LockKey a))

data Lockable a = Locked (LockKey a -> Bool) a | Unlocked a

buildLock :: (HasLock a, b ~ LockKey a, Eq b) => a -> Lockable a
buildLock a = case a ^. lock of
  Just lock' -> Locked (== lock') a
  Nothing -> Unlocked a

addLock :: (HasLock a, b ~ LockKey a, Eq b) => b -> a -> Lockable a
addLock b a = Locked (== b) $ a & lock ?~ b

runLocked
  :: (HasLock a, b ~ LockKey a)
  => b
  -> (a -> Lockable a)
  -> Lockable a
  -> Lockable a
runLocked _ f (Unlocked a) = f a
runLocked key f (Locked lock' a) | lock' key = f a
runLocked _ _ l = l

runLockedM
  :: (HasLock a, b ~ LockKey a, Monad m)
  => b
  -> (a -> m (Lockable a))
  -> Lockable a
  -> m (Lockable a)
runLockedM _ f (Unlocked a) = f a
runLockedM key f (Locked lock' a) | lock' key = f a
runLockedM _ _ l = pure l

-- |Occasionally we want to be able to adjust state
-- |independent of the lock, but such an operation
-- |should retain the original Lockable state,
-- |therefore we use a function from (a -> a) instead
-- |of the usual (a -> Locked a).
runIgnoreLocked :: (HasLock a) => (a -> a) -> Lockable a -> Lockable a
runIgnoreLocked f (Unlocked a) = Unlocked (f a)
runIgnoreLocked f (Locked lock' a) = Locked lock' (f a)

runIgnoreLockedM
  :: (HasLock a, Functor m) => (a -> m a) -> Lockable a -> m (Lockable a)
runIgnoreLockedM f (Unlocked a) = Unlocked <$> f a
runIgnoreLockedM f (Locked lock' a) = Locked lock' <$> f a

runOnlyLocked
  :: (HasLock a, b ~ LockKey a)
  => b
  -> (a -> Lockable a)
  -> Lockable a
  -> Lockable a
runOnlyLocked key f (Locked lock' a) | lock' key = f a
runOnlyLocked _ _ l = l

runOnlyUnlocked :: (a -> Lockable a) -> Lockable a -> Lockable a
runOnlyUnlocked f (Unlocked a) = f a
runOnlyUnlocked _ l = l

runOnlyUnlockedM
  :: Monad m => (a -> m (Lockable a)) -> Lockable a -> m (Lockable a)
runOnlyUnlockedM f (Unlocked a) = f a
runOnlyUnlockedM _ l = pure l

removeLock :: (HasLock a) => Lockable a -> a
removeLock a = withoutLock a & lock .~ Nothing

withoutLock :: (HasLock a) => Lockable a -> a
withoutLock (Locked _ a) = a
withoutLock (Unlocked a) = a

withoutLockM :: (Monad m, HasLock a) => Lockable a -> m a
withoutLockM (Locked _ a) = pure a
withoutLockM (Unlocked a) = pure a

isLocked :: (HasLock a) => Lockable a -> Bool
isLocked (Locked _ _) = True
isLocked (Unlocked _) = False
