{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Token where

import Arkham.Prelude hiding (Index)
import Control.Lens.At
import GHC.OverloadedLabels

data Token
  = Resource
  | Damage
  | Horror
  | Clue
  | Doom
  | LostSoul
  | Bounty
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

instance IsLabel "damage" Token where
  fromLabel = Damage

instance IsLabel "horror" Token where
  fromLabel = Horror

instance IsLabel "resource" Token where
  fromLabel = Resource

newtype Tokens = Tokens (Map Token Int)
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, Monoid, Semigroup)

type instance Index Tokens = Token
type instance IxValue Tokens = Int

instance Ixed Tokens where
  ix token f (Tokens tokens) = Tokens <$> ix token f tokens
instance At Tokens where
  at token f (Tokens tokens) = Tokens <$> at token f tokens

flipClues :: Int -> Tokens -> Tokens
flipClues n original@(Tokens tokens) =
  let clueVal = findWithDefault 0 Clue tokens
      n' = min n clueVal
   in if clueVal == 0 then original else addTokens Doom n' (subtractTokens Clue n' original)

flipDoom :: Int -> Tokens -> Tokens
flipDoom n original@(Tokens tokens) =
  let doomVal = findWithDefault 0 Doom tokens
      n' = min n doomVal
   in if doomVal == 0 then original else addTokens Clue n' (subtractTokens Doom n' original)

countTokens :: Token -> Tokens -> Int
countTokens token (Tokens tokens) = findWithDefault 0 token tokens

addTokens :: Token -> Int -> Tokens -> Tokens
addTokens token amount (Tokens tokens) =
  let val = max 0 (findWithDefault 0 token tokens + amount)
   in if val == 0
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
