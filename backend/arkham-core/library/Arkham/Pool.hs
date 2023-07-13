module Arkham.Pool where

import Arkham.Prelude

data PoolToken = Resource | Damage | Horror | Clue | Doom
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype Tokens = Tokens (Map PoolToken Int)
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

countTokens :: PoolToken -> Tokens -> Int
countTokens token (Tokens tokens) = findWithDefault 0 token tokens

addTokens :: PoolToken -> Int -> Tokens -> Tokens
addTokens token amount (Tokens tokens) =
  Tokens $ insertWith (+) token amount tokens

increateTokens :: PoolToken -> Tokens -> Tokens
increateTokens token = addTokens token 1

descreaseTokens :: PoolToken -> Tokens -> Tokens
descreaseTokens token = subtractTokens token 1

subtractTokens :: PoolToken -> Int -> Tokens -> Tokens
subtractTokens token amount ts@(Tokens tokens) =
  let val = max 0 (countTokens token ts - amount)
  in  Tokens $ insertMap token val tokens
