module Arkham.Source where

import Arkham.Prelude

data Source

instance Data Source
instance ToJSON Source
instance FromJSON Source
instance Eq Source
instance Ord Source
instance Show Source

class Sourceable a where
  toSource :: a -> Source
  isSource :: a -> Source -> Bool
  isSource = (==) . toSource
  {-# MINIMAL toSource #-}
