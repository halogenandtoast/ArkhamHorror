module Api.Handler.Arkham.Old (
  getApiV1ArkhamGamesOldR,
) where

import Control.Lens hiding (Choice, op)
import Control.Monad (foldM)
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Lens ()
import Data.Aeson.Patch
import Data.Aeson.Patch qualified as Patch
import Data.Map.Strict qualified as Map
import Data.UUID
import Database.Persist qualified as Persist
import Entity.Arkham.GameRaw
import Entity.Arkham.Step
import Import hiding (State, runState)

getApiV1ArkhamGamesOldR :: Handler ()
getApiV1ArkhamGamesOldR = runDB $ do
  gameIds <- selectKeysList @ArkhamGameRaw [] []
  for_ gameIds $ \gameId -> do
    ArkhamGameRaw {..} <- get404 gameId
    let (newJsonValue, uuidMap, counter) = processJSONWithExistingMap arkhamGameRawCurrentData Map.empty 0
    Persist.update gameId [ArkhamGameRawCurrentData =. newJsonValue]

    stepIds <- selectKeysList @ArkhamStep [ArkhamStepArkhamGameId ==. coerce gameId] []
    -- for_ stepIds $ \stepId -> do
    --   ArkhamStep {..} <- get404 stepId
    --   let choice' = updateChoice uuidMap arkhamStepChoice
    --   Persist.update stepId [ArkhamStepChoice =. choice']

    (_, _) <-
      foldM
        ( \(uuidMapAcc, counterAcc) stepId -> do
            ArkhamStep {..} <- get404 stepId
            let (choice', uuidMapAcc', counterAcc') = updateChoice uuidMapAcc counterAcc arkhamStepChoice
            Persist.update stepId [ArkhamStepChoice =. choice']
            return (uuidMapAcc', counterAcc')
        )
        (uuidMap, counter)
        stepIds
    pure ()

-- Update the Choice, returning the updated UUIDMap and Counter
updateChoice :: UUIDMap -> Counter -> Choice -> (Choice, UUIDMap, Counter)
updateChoice uuidMap counter choice =
  let (patchDown', uuidMap', counter') = updatePatch uuidMap counter (choicePatchDown choice)
   in (choice {choicePatchDown = patchDown'}, uuidMap', counter')

-- Update the Patch, processing each operation
updatePatch :: UUIDMap -> Counter -> Patch -> (Patch, UUIDMap, Counter)
updatePatch uuidMap counter (Patch ops) =
  let (ops', uuidMap', counter') =
        foldl'
          ( \(opsAcc, uuidMapAcc, counterAcc) op ->
              let (op', uuidMapAcc', counterAcc') = updateOperation uuidMapAcc counterAcc op
               in (opsAcc ++ [op'], uuidMapAcc', counterAcc')
          )
          ([], uuidMap, counter)
          ops
   in (Patch ops', uuidMap', counter')

-- Update each Operation, replacing UUIDs in values
updateOperation :: UUIDMap -> Counter -> Operation -> (Operation, UUIDMap, Counter)
updateOperation uuidMap counter op = case op of
  Patch.Add ptr val ->
    let (val', uuidMap', counter') = replaceUUIDs uuidMap counter val
     in (Patch.Add ptr val', uuidMap', counter')
  Rep ptr val ->
    let (val', uuidMap', counter') = replaceUUIDs uuidMap counter val
     in (Rep ptr val', uuidMap', counter')
  Tst ptr val ->
    let (val', uuidMap', counter') = replaceUUIDs uuidMap counter val
     in (Tst ptr val', uuidMap', counter')
  other -> (other, uuidMap, counter)

-- Define types for the UUID mapping and counter
type UUIDMap = Map UUID Int
type Counter = Int
type UUIDState = State (UUIDMap, Counter)

-- Replace UUIDs in a Value, updating the UUIDMap and Counter
replaceUUIDs :: UUIDMap -> Counter -> Value -> (Value, UUIDMap, Counter)
replaceUUIDs uuidMap counter val =
  let (newVal, (uuidMap', counter')) = runState (processJSONState val) (uuidMap, counter)
   in (newVal, uuidMap', counter')

-- Function to process JSON with an existing UUIDMap and Counter
processJSONWithExistingMap :: Value -> UUIDMap -> Counter -> (Value, UUIDMap, Counter)
processJSONWithExistingMap jsonValue initialMap initialCounter =
  let (newJsonValue, (uuidMap, counter)) = runState (processJSONState jsonValue) (initialMap, initialCounter)
   in (newJsonValue, uuidMap, counter)

processJSONState :: Value -> UUIDState Value
processJSONState = transformM replaceFunc

replaceFunc :: Value -> UUIDState Value
replaceFunc (String s) =
  case fromText s of
    Just uuid -> do
      (uuidMap, counter) <- get
      case Map.lookup uuid uuidMap of
        Just i -> return $ Number (fromIntegral i)
        Nothing -> do
          let i = counter + 1
          put (Map.insert uuid i uuidMap, i)
          return $ Number (fromIntegral i)
    Nothing -> return $ String s
replaceFunc v = return v
