{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Utility where

import Data.Aeson
import qualified Data.Char as C
import Data.List (isPrefixOf)
import qualified Data.List as List
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Prelude
  ( Maybe(..)
  , Monad
  , String
  , drop
  , error
  , id
  , length
  , maybe
  , otherwise
  , pure
  , show
  , ($)
  , (.)
  , (<>)
  )

newtype Codec (tag :: k) (val :: *) = Codec val
data Drop something

newtype GenericToJSON value = GenericToJSON value

instance (GToEncoding Zero (Rep value), GToJSON Zero (Rep value), Generic value) => ToJSON (GenericToJSON value) where
  toJSON (GenericToJSON value) = genericToJSON defaultOptions value
  toEncoding (GenericToJSON value) = genericToEncoding defaultOptions value

instance (GToEncoding Zero (Rep a), GToJSON Zero (Rep a), Generic a, ModifyOptions tag) => ToJSON (Codec tag a) where
  toJSON (Codec a) = genericToJSON (modifyOptions @tag defaultOptions) a
  toEncoding (Codec a) =
    genericToEncoding (modifyOptions @tag defaultOptions) a

class ModifyOptions tag where
  modifyOptions :: Options -> Options

addFieldLabelModifier :: (String -> String) -> Options -> Options
addFieldLabelModifier f options =
  options { fieldLabelModifier = f . fieldLabelModifier options }

addConstructorTagModifier :: (String -> String) -> Options -> Options
addConstructorTagModifier f options =
  options { constructorTagModifier = f . constructorTagModifier options }

instance (KnownSymbol symbol) => ModifyOptions (Drop symbol) where
  modifyOptions =
    addConstructorTagModifier
        (\constructorTag -> case List.stripPrefix prefix constructorTag of
          Just striped -> snakeCaseify $ unCapitalize striped
          Nothing -> constructorTag
        )
      . addFieldLabelModifier
          (\fieldLabel ->
            maybe fieldLabel unCapitalize (List.stripPrefix prefix fieldLabel)
          )
    where prefix = symbolVal (Proxy @symbol)


snakeCaseify :: String -> String
snakeCaseify [] = []
snakeCaseify (c : cs)
  | C.isLower c = c : snakeCaseify cs
  | otherwise = '_' : C.toLower c : snakeCaseify cs

unCapitalize :: String -> String
unCapitalize [] = []
unCapitalize (c : cs) = C.toLower c : cs

aesonOptions :: Maybe String -> Options
aesonOptions mPrefix = defaultOptions
  { constructorTagModifier = snakeCaseify . unCapitalize . modify
  , fieldLabelModifier = unCapitalize . modify
  }
  where modify = maybe id dropPrefix mPrefix

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM nothingAction = maybe nothingAction pure

dropPrefix :: String -> String -> String
dropPrefix prefix x = if prefix `isPrefixOf` x
  then drop (length prefix) x
  else error $ "dropPrefix: " <> show prefix <> " is not a prefix of " <> show x
