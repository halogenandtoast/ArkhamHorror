{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Act
  ( Act(..)
  , lookupAct
  ) where

import Arkham.Prelude hiding ( fold )

import Arkham.Act.Acts
import Arkham.Act.Types
import Arkham.Card
import Arkham.Id

lookupAct :: ActId -> (Int -> Act)
lookupAct actId = case lookup (unActId actId) allActs of
  Nothing -> error $ "Unknown act: " <> show actId
  Just (SomeActCard a) -> \i -> Act $ cbCardBuilder a (i, actId)

instance FromJSON Act where
  parseJSON v = flip (withObject "Act") v $ \o -> do
    cCode :: CardCode <- o .: "id"
    withActCardCode cCode $ \(_ :: ActCard a) -> Act <$> parseJSON @a v

withActCardCode :: CardCode -> (forall a . IsAct a => ActCard a -> r) -> r
withActCardCode cCode f = case lookup cCode allActs of
  Nothing -> error $ "Unknown act: " <> show cCode
  Just (SomeActCard a) -> f a

allActs :: HashMap CardCode SomeActCard
allActs = mapFromList $ map
  (toFst someActCardCode)
  [ -- Night of the Zealot
  -- The Gathering
    SomeActCard trapped
  , SomeActCard theBarrier
  , SomeActCard whatHaveYouDone
  ]
