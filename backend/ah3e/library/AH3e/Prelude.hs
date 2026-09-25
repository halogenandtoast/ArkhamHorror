module AH3e.Prelude (
  module X,
  Text,
  Map,
  Set,
  tshow,
  fromJustNote,
  (<&>),
) where

import Control.Lens as X (
  Lens',
  Traversal',
  at,
  each,
  filtered,
  has,
  ix,
  non,
  over,
  preuse,
  set,
  singular,
  to,
  toListOf,
  traversed,
  use,
  uses,
  view,
  (%=),
  (%~),
  (&),
  (+=),
  (+~),
  (-=),
  (-~),
  (.=),
  (.~),
  (<>=),
  (<>~),
  (<~),
  (?=),
  (?~),
  (^.),
  (^..),
  (^?),
  _1,
  _2,
  _Just,
  (||~),
 )
import Control.Monad as X
import Control.Monad.State.Strict as X (
  MonadState,
  State,
  StateT,
  execState,
  get,
  gets,
  modify',
  put,
  runState,
 )
import Data.Aeson as X (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Bifunctor as X (first, second)
import Data.Coerce as X (coerce)
import Data.Foldable as X (for_, toList, traverse_)
import Data.Functor ((<&>))
import Data.Generics.Labels as X ()
import Data.List as X (sortOn, (\\))
import Data.Map.Strict (Map)
import Data.Maybe as X (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.String as X (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable as X (for)
import GHC.Generics as X (Generic)
import GHC.Stack as X (HasCallStack)
import Prelude as X

tshow :: Show a => a -> Text
tshow = T.pack . show

fromJustNote :: HasCallStack => String -> Maybe a -> a
fromJustNote note = fromMaybe (error note)
