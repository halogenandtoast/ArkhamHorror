{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Token where

import Arkham.Prelude

-- We can place tokens "AS" other things

data Token
  = Resource
  | Damage
  | Horror
  | Clue
  | Doom
  | LostSoul
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype Tokens = Tokens (Map Token Int)
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, Monoid, Semigroup)

flipClues :: Int -> Tokens -> Tokens
flipClues n original@(Tokens tokens) =
  let clueVal = findWithDefault 0 Clue tokens
      n' = min n clueVal
  in  if clueVal == 0 then original else addTokens Doom n' (subtractTokens Clue n' original)

countTokens :: Token -> Tokens -> Int
countTokens token (Tokens tokens) = findWithDefault 0 token tokens

addTokens :: Token -> Int -> Tokens -> Tokens
addTokens token amount (Tokens tokens) =
  let val = max 0 (findWithDefault 0 token tokens + amount)
  in  if val == 0
        then Tokens $ deleteMap token tokens
        else Tokens $ insertMap token val tokens

removeAllTokens :: Token -> Tokens -> Tokens
removeAllTokens token (Tokens tokens) = Tokens $ deleteMap token tokens

incrementTokens :: Token -> Tokens -> Tokens
incrementTokens token = addTokens token 1

decrementTokens :: Token -> Tokens -> Tokens
decrementTokens token = subtractTokens token 1

subtractTokens :: Token -> Int -> Tokens -> Tokens
subtractTokens token amount = addTokens token (-amount)

setTokens :: Token -> Int -> Tokens -> Tokens
setTokens token (max 0 -> amount) (Tokens tokens)
  | amount > 0 = Tokens $ insertMap token amount tokens
  | otherwise = Tokens $ deleteMap token tokens
