module Base.Lock where

import ClassyPrelude
import Lens.Micro

class HasLock a where
  type LockKey a
  lock :: Lens' a (Maybe (LockKey a))

data Lockable b a = Locked (b -> Bool) a | Unlocked a

buildLock :: (HasLock a, b ~ LockKey a, Eq b) => a -> Lockable b a
buildLock a = case a ^. lock of
  Just lock' -> Locked (== lock') a
  Nothing -> Unlocked a

addLock :: (HasLock a, b ~ LockKey a, Eq b) => b -> a -> Lockable b a
addLock b a = Locked (== b) $ a & lock ?~ b

runLocked
  :: (HasLock a, b ~ LockKey a)
  => b
  -> (a -> Lockable b a)
  -> Lockable b a
  -> Lockable b a
runLocked _ f (Unlocked a) = f a
runLocked key f (Locked lock' a) | lock' key = f a
runLocked _ _ l = l

runOnlyLocked
  :: (HasLock a, b ~ LockKey a)
  => b
  -> (a -> Lockable b a)
  -> Lockable b a
  -> Lockable b a
runOnlyLocked key f (Locked lock' a) | lock' key = f a
runOnlyLocked _ _ l = l

removeLock :: (HasLock a, b ~ LockKey a) => Lockable b a -> a
removeLock a = withoutLock a & lock .~ Nothing

withoutLock :: (HasLock a, b ~ LockKey a) => Lockable b a -> a
withoutLock (Locked _ a) = a
withoutLock (Unlocked a) = a

isLocked :: (HasLock a, b ~ LockKey a) => Lockable b a -> Bool
isLocked (Locked _ _) = True
isLocked (Unlocked _) = False
