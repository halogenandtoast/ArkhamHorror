module Arkham.Game.Diff where

import Arkham.Prelude

import Arkham.Game.Base
import Arkham.Game.Json ()
import Data.Aeson
import Data.Aeson.Diff qualified as Diff
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Patch (Operation (..), Patch (..), modifyPointer)
import Data.Aeson.Pointer (Key (..), Pointer (..))
import Data.Vector qualified as V

-- We need to exclude gameActionDiff since it will cause a very large diff
-- We will just copy it directly
diff :: Game -> Game -> Diff.Patch
diff a b = Diff.diff (toJSON (a {gameActionDiff = []})) (toJSON (b {gameActionDiff = []}))

patch :: Game -> Diff.Patch -> Result Game
patch g p = case Diff.patch p (toJSON g) of
  Error e -> Error e
  Success a -> fromJSON a

descend :: Value -> Data.Aeson.Pointer.Key -> Value
descend (Object obj) (OKey k) = fromMaybe Null $ KM.lookup k obj
descend (Array arr) (AKey n) = fromMaybe Null $ arr V.!? n
descend _ _ = Null

-- How the path was transformed when walking through an Actions tagged union.
data RecoveryKind
  = NoRecovery
  | -- AndActions / OrActions: injected "contents" before the array index.
    -- Value needs to be promoted from a plain Action string to SingleAction.
    AndActionsKind
  | -- SingleAction: replaced AKey n with OKey "contents" (no array involved).
    -- For Remove this means the whole actions node should become empty AndActions.
    SingleActionKind

{- | Walk a pointer path alongside the current JSON value, rewriting steps
that would land on an Actions tagged-union instead of the expected array.
Returns the rewritten path and what kind of recovery (if any) occurred.
-}
recoverPath :: Value -> [Data.Aeson.Pointer.Key] -> ([Data.Aeson.Pointer.Key], RecoveryKind)
recoverPath _ [] = ([], NoRecovery)
recoverPath v (OKey k : rest) =
  let (rest', kind) = recoverPath (descend v (OKey k)) rest
   in (OKey k : rest', kind)
recoverPath v (AKey n : rest) =
  case v of
    Object obj ->
      case (KM.lookup "tag" obj, KM.lookup "contents" obj) of
        -- SingleAction: contents is a scalar, not an array.
        -- AKey n → OKey "contents" (drops the index).
        (Just (String "SingleAction"), _) ->
          let (rest', _) = recoverPath (descend v (OKey "contents")) rest
           in (OKey "contents" : rest', SingleActionKind)
        -- AndActions / OrActions (or any other tagged union whose contents IS an array).
        -- AKey n → OKey "contents", AKey n.
        (_, Just (Array arr)) ->
          let (rest', _) = recoverPath (fromMaybe Null $ arr V.!? n) rest
           in (OKey "contents" : AKey n : rest', AndActionsKind)
        _ ->
          let (rest', kind) = recoverPath (descend v (AKey n)) rest
           in (AKey n : rest', kind)
    _ ->
      let (rest', kind) = recoverPath (descend v (AKey n)) rest
       in (AKey n : rest', kind)

recoverPointer :: Value -> Pointer -> Pointer
recoverPointer v (Pointer path) = Pointer (fst $ recoverPath v path)

emptyAndActions :: Value
emptyAndActions = object ["tag" .= ("AndActions" :: Text), "contents" .= ([] :: [Value])]

-- Old format stored Action strings directly (e.g. "Investigate").
-- New format wraps them as SingleAction inside the Actions tagged union.
promoteAction :: Value -> Value
promoteAction (String s) = object ["tag" .= ("SingleAction" :: Text), "contents" .= s]
promoteAction val = val

recoverOperation :: Value -> Operation -> Operation
recoverOperation v op = case op of
  Add {} ->
    let (newPath, kind) = recoverPath v (pointerPath $ changePointer op)
     in op
          { changePointer = Pointer newPath
          , changeValue = case kind of
              AndActionsKind -> promoteAction (changeValue op)
              _ -> changeValue op
          }
  Rep {} ->
    let (newPath, kind) = recoverPath v (pointerPath $ changePointer op)
     in op
          { changePointer = Pointer newPath
          , changeValue = case kind of
              AndActionsKind -> promoteAction (changeValue op)
              _ -> changeValue op
          }
  Rem {} ->
    let (newPath, kind) = recoverPath v (pointerPath $ changePointer op)
     in case kind of
          -- SingleAction has no array; removing its only element means replacing
          -- the whole actions node with an empty AndActions.
          -- recoverPath appended OKey "contents" at the end — drop it to get the
          -- parent path, then emit a Replace rather than a Remove.
          SingleActionKind -> Rep (Pointer $ take (length newPath - 1) newPath) emptyAndActions
          _ -> op {changePointer = Pointer newPath}
  _ -> modifyPointer (recoverPointer v) op

-- Apply each operation individually so that when one fails we recover its
-- pointer (and value) against the *current intermediate* JSON, not the
-- original game.  This matters when earlier ops (e.g. removes) shift array
-- elements before the op that hits an AndActions object.
patchWithRecovery :: Game -> Diff.Patch -> Result Game
patchWithRecovery g (Patch ops) =
  case foldM applyWithRecovery (toJSON g) ops of
    Error e -> Error e
    Success v -> fromJSON v

-- | Apply a patch directly to a JSON Value, avoiding the expensive Game<->Value
-- round-trip. Use this when you already have the game state as a Value (e.g.
-- fetched via ArkhamGameRaw) to avoid two full serialization cycles.
patchValueWithRecovery :: Value -> Diff.Patch -> Result Value
patchValueWithRecovery v (Patch ops) = foldM applyWithRecovery v ops

-- | Strip the gameActionDiff field from a JSON Value before diffing.
-- Avoids including the large (and irrelevant) action diff in the patch.
stripActionDiff :: Value -> Value
stripActionDiff (Object obj) = Object (KM.delete "gameActionDiff" obj)
stripActionDiff v = v

-- | Compute a diff directly from pre-serialized Values, avoiding Game<->Value
-- round-trips. Equivalent to 'diff' but works at the Value level.
-- Use this when you already have both game states as Values (e.g. fetched via
-- ArkhamGameRaw and the result of toJSON after runMessages).
diffValues :: Value -> Value -> Diff.Patch
diffValues a b = Diff.diff (stripActionDiff a) (stripActionDiff b)

-- | Update the gameSeed field directly in a JSON Value without going through Game.
setGameSeed :: Int -> Value -> Value
setGameSeed seed (Object obj) = Object $ KM.insert "gameSeed" (toJSON seed) obj
setGameSeed _ v = v

applyWithRecovery :: Value -> Operation -> Result Value
applyWithRecovery v op =
  case Diff.applyOperation op v of
    Error _ -> Diff.applyOperation (recoverOperation v op) v
    ok -> ok

unsafePatch :: Game -> Diff.Patch -> Game
unsafePatch g p = case patchWithRecovery g p of
  Success a -> a
  _ -> error "Could not patch safely"
