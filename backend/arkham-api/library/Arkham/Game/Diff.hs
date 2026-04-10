module Arkham.Game.Diff where

import Arkham.Prelude

import Arkham.Game.Base
import Arkham.Game.Json ()
import Data.Aeson
import Data.Aeson.Diff qualified as Diff
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Patch (Operation(..), Patch (..), modifyPointer)
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

-- | Walk a pointer path alongside the current JSON value.  Wherever an AKey
-- step would land on an Object with a "contents" array (the AndActions
-- tagged-union shape), inject OKey "contents" before the index.
-- Returns the fixed path and True if any injection occurred.
recoverPath :: Value -> [Data.Aeson.Pointer.Key] -> ([Data.Aeson.Pointer.Key], Bool)
recoverPath _ [] = ([], False)
recoverPath v (OKey k : rest) =
  let (rest', flag) = recoverPath (descend v (OKey k)) rest
  in (OKey k : rest', flag)
recoverPath v (AKey n : rest) =
  case v of
    Object obj
      | Just (Array arr) <- KM.lookup "contents" obj ->
          let (rest', _) = recoverPath (fromMaybe Null $ arr V.!? n) rest
          in (OKey "contents" : AKey n : rest', True)
    _ ->
      let (rest', flag) = recoverPath (descend v (AKey n)) rest
      in (AKey n : rest', flag)

recoverPointer :: Value -> Pointer -> Pointer
recoverPointer v (Pointer path) = Pointer (fst $ recoverPath v path)

-- When an op is being recovered and the pointer was adjusted for AndActions,
-- also promote any bare string value to {"tag": s} (the new tagged encoding).
recoverOperation :: Value -> Operation -> Operation
recoverOperation v op = case op of
  Add {} ->
    let (newPath, injected) = recoverPath v (pointerPath $ changePointer op)
    in op
        { Diff.changePointer = Pointer newPath
        , changeValue = if injected then promoteAction (changeValue op) else changeValue op
        }
  Rep {} ->
    let (newPath, injected) = recoverPath v (pointerPath $ changePointer op)
    in op
        { changePointer = Pointer newPath
        , changeValue = if injected then promoteAction (changeValue op) else changeValue op
        }
  _ -> modifyPointer (recoverPointer v) op

-- Old format stored Action strings directly (e.g. "Investigate").
-- New format wraps them as SingleAction inside the Actions tagged union.
promoteAction :: Value -> Value
promoteAction (String s) = object ["tag" .= ("SingleAction" :: Text), "contents" .= s]
promoteAction val = val

-- Apply each operation individually so that when one fails we recover its
-- pointer (and value) against the *current intermediate* JSON, not the
-- original game.  This matters when earlier ops (e.g. removes) shift array
-- elements before the op that hits an AndActions object.
patchWithRecovery :: Game -> Diff.Patch -> Result Game
patchWithRecovery g (Patch ops) =
  case foldM applyWithRecovery (toJSON g) ops of
    Error e -> Error e
    Success v -> fromJSON v

applyWithRecovery :: Value -> Operation -> Result Value
applyWithRecovery v op =
  case Diff.applyOperation op v of
    Error _ -> Diff.applyOperation (recoverOperation v op) v
    ok -> ok

unsafePatch :: Game -> Diff.Patch -> Game
unsafePatch g p = case patchWithRecovery g p of
  Success a -> a
  _ -> error "Could not patch safely"
