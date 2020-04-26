{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Arkham.Model.ProductSet where

import Data.Text
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Prelude (Show)

mkPersist sqlSettings [persistLowerCase|
ArkhamProductSet sql=arkham_product_sets
  title Text
  type Text
  UniqueProductSet title
  deriving Show Generic
|]
