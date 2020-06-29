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

runOnlyLocked
  :: (HasLock a, b ~ LockKey a)
  => b
  -> (a -> Lockable a)
  -> Lockable a
  -> Lockable a
runOnlyLocked key f (Locked lock' a) | lock' key = f a
runOnlyLocked _ _ l = l

removeLock :: (HasLock a) => Lockable a -> a
removeLock a = withoutLock a & lock .~ Nothing

withoutLock :: (HasLock a) => Lockable a -> a
withoutLock (Locked _ a) = a
withoutLock (Unlocked a) = a

isLocked :: (HasLock a) => Lockable a -> Bool
isLocked (Locked _ _) = True
isLocked (Unlocked _) = False
