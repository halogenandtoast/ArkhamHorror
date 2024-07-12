{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Token where

import Arkham.Prelude hiding (Index)
import GHC.OverloadedLabels

-- Make sure to update src/arkham/types/Token.ts
data Token
  = Aether
  | AlarmLevel
  | Ammo
  | Bounty
  | Charge
  | Clue
  | Corruption
  | Damage
  | Depth
  | Doom
  | Durability
  | Evidence
  | Horror
  | Key
  | Leyline
  | Lock
  | LostSoul
  | Offering
  | Resource
  | Secret
  | Supply
  | Time
  | Try
  | Whistle
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

tokenIsUse :: Token -> Bool
tokenIsUse = \case
  Damage -> False
  Horror -> False
  Clue -> False
  Doom -> False
  _ -> True

instance IsLabel "damage" Token where
  fromLabel = Damage

instance IsLabel "horror" Token where
  fromLabel = Horror

instance IsLabel "resource" Token where
  fromLabel = Resource

instance IsLabel "clue" Token where
  fromLabel = Clue

instance IsLabel "doom" Token where
  fromLabel = Doom

type Tokens = Map Token Int

flipClues :: Int -> Tokens -> Tokens
flipClues n tokens =
  let clueVal = findWithDefault 0 Clue tokens
      n' = min n clueVal
   in if clueVal == 0 then tokens else addTokens Doom n' (subtractTokens Clue n' tokens)

flipDoom :: Int -> Tokens -> Tokens
flipDoom n tokens =
  let doomVal = findWithDefault 0 Doom tokens
      n' = min n doomVal
   in if doomVal == 0 then tokens else addTokens Clue n' (subtractTokens Doom n' tokens)

hasToken :: Token -> Tokens -> Bool
hasToken token tokens = countTokens token tokens > 0

countTokens :: Token -> Tokens -> Int
countTokens token tokens = findWithDefault 0 token tokens

addTokens :: Token -> Int -> Tokens -> Tokens
addTokens token amount tokens =
  let val = max 0 (findWithDefault 0 token tokens + amount)
   in if val == 0
        then deleteMap token tokens
        else insertMap token val tokens

removeAllTokens :: Token -> Tokens -> Tokens
removeAllTokens token tokens = deleteMap token tokens

incrementTokens :: Token -> Tokens -> Tokens
incrementTokens token = addTokens token 1

decrementTokens :: Token -> Tokens -> Tokens
decrementTokens token = decrementTokensBy token 1

decrementTokensBy :: Token -> Int -> Tokens -> Tokens
decrementTokensBy token n = subtractTokens token n

subtractTokens :: Token -> Int -> Tokens -> Tokens
subtractTokens token amount = addTokens token (-amount)

setTokens :: Token -> Int -> Tokens -> Tokens
setTokens token (max 0 -> amount) tokens
  | amount > 0 = insertMap token amount tokens
  | otherwise = deleteMap token tokens
