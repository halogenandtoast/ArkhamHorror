{-# LANGUAGE TemplateHaskell #-}

module Arkham.Asset.Uses where

import Arkham.Prelude

import Data.Aeson.TH

data UseType
  = Ammo
  | Supply
  | Secret
  | Charge
  | Try
  | Bounty
  | Whistle
  | Resource
  | Key
  | Lock
  | Evidence
  | Offering
  | Leyline
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''UseType)

deriving anyclass instance ToJSONKey UseType
deriving anyclass instance FromJSONKey UseType

data Uses n = NoUses | Uses UseType n | UsesWithLimit UseType n n
  deriving stock (Show, Eq, Ord, Data)

use :: Uses Int -> Uses Int
use = useN 1

useN :: Int -> Uses Int -> Uses Int
useN _ NoUses = NoUses
useN n (Uses useType' m) = Uses useType' (max 0 (m - n))
useN n (UsesWithLimit useType' m l) = UsesWithLimit useType' (max 0 (m - n)) l

useType :: Uses n -> Maybe UseType
useType NoUses = Nothing
useType (Uses useType' _) = Just useType'
useType (UsesWithLimit useType' _ _) = Just useType'

useCount :: Uses Int -> Int
useCount NoUses = 0
useCount (Uses _ n) = n
useCount (UsesWithLimit _ n _) = n

useTypeCount :: UseType -> Uses Int -> Int
useTypeCount _ NoUses = 0
useTypeCount u (Uses v n) = if u == v then n else 0
useTypeCount u (UsesWithLimit v n _) = if u == v then n else 0

instance ToJSON n => ToJSON (Uses n) where
  toJSON NoUses = Null
  toJSON (Uses t n) = object ["type" .= toJSON t, "amount" .= toJSON n]
  toJSON (UsesWithLimit t n l) =
    object ["type" .= toJSON t, "amount" .= toJSON n, "limit" .= toJSON l]
  toEncoding NoUses = toEncoding Null
  toEncoding (Uses t n) = pairs ("type" .= t <> "amount" .= n)
  toEncoding (UsesWithLimit t n l) =
    pairs ("type" .= t <> "amount" .= n <> "limit" .= l)

instance FromJSON n => FromJSON (Uses n) where
  parseJSON = \case
    Null -> pure NoUses
    Object o ->
      (UsesWithLimit <$> o .: "type" <*> o .: "amount" <*> o .: "limit")
        <|> (Uses <$> o .: "type" <*> o .: "amount")
    _ -> error "no such parse"
