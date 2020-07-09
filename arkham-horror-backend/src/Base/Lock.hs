module Base.Lock where

import ClassyPrelude
import Lens.Micro

class Unlock a where
  type Key a
  unlocks :: a -> Key a -> Bool

class (Unlock (Lock a)) => HasLock a where
  type Lock a
  lock :: Lens' a (Maybe (Lock a))

data Lockable a = Locked (Lock a) a | Unlocked a

buildLock :: (HasLock a, b ~ Lock a) => a -> Lockable a
buildLock a = case a ^. lock of
  Just lock' -> Locked lock' a
  Nothing -> Unlocked a

addLock :: (HasLock a, b ~ Lock a, Eq b) => b -> a -> Lockable a
addLock b a = Locked b $ a & lock ?~ b

runLocked
  :: (HasLock a, b ~ Key (Lock a))
  => b
  -> (a -> Lockable a)
  -> Lockable a
  -> Lockable a
runLocked _ f (Unlocked a) = f a
runLocked key f (Locked lock' a) | unlocks lock' key = f a
runLocked _ _ l = l

runLockedM
  :: (HasLock a, b ~ Key (Lock a), Monad m)
  => b
  -> (a -> m (Lockable a))
  -> Lockable a
  -> m (Lockable a)
runLockedM _ f (Unlocked a) = f a
runLockedM key f (Locked lock' a) | unlocks lock' key = f a
runLockedM _ _ l = pure l

runLockedWithLock
  :: (HasLock a, b ~ Key (Lock a))
  => b
  -> (a -> Maybe (Lock a) -> Lockable a)
  -> Lockable a
  -> Lockable a
runLockedWithLock _ f (Unlocked a) = f a Nothing
runLockedWithLock key f (Locked lock' a) | unlocks lock' key = f a (Just lock')
runLockedWithLock _ _ l = l

runLockedWithLockM
  :: (HasLock a, b ~ Key (Lock a), Monad m)
  => b
  -> (a -> Maybe (Lock a) -> m (Lockable a))
  -> Lockable a
  -> m (Lockable a)
runLockedWithLockM _ f (Unlocked a) = f a Nothing
runLockedWithLockM key f (Locked lock' a) | unlocks lock' key = f a (Just lock')
runLockedWithLockM _ _ l = pure l

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
  :: (HasLock a, b ~ Key (Lock a))
  => b
  -> (a -> Lockable a)
  -> Lockable a
  -> Lockable a
runOnlyLocked key f (Locked lock' a) | unlocks lock' key = f a
runOnlyLocked _ _ l = l

runOnlyLockedM
  :: (HasLock a, b ~ Key (Lock a), Monad m)
  => b
  -> (a -> m (Lockable a))
  -> Lockable a
  -> m (Lockable a)
runOnlyLockedM key f (Locked lock' a) | unlocks lock' key = f a
runOnlyLockedM _ _ l = pure l

runOnlyLockedWithLock
  :: (HasLock a, b ~ Key (Lock a))
  => b
  -> (a -> Lock a -> Lockable a)
  -> Lockable a
  -> Lockable a
runOnlyLockedWithLock key f (Locked lock' a) | unlocks lock' key = f a lock'
runOnlyLockedWithLock _ _ l = l

runOnlyLockedWithLockM
  :: (HasLock a, b ~ Key (Lock a), Monad m)
  => b
  -> (a -> Lock a -> m (Lockable a))
  -> Lockable a
  -> m (Lockable a)
runOnlyLockedWithLockM key f (Locked lock' a) | unlocks lock' key = f a lock'
runOnlyLockedWithLockM _ _ l = pure l

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
