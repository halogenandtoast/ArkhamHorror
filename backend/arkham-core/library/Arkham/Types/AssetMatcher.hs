module Arkham.Types.AssetMatcher where

import Arkham.Prelude

import Arkham.Types.Id

import Arkham.Types.Asset.Uses
import Arkham.Types.Trait

data AssetMatcher
  = AssetWithTitle Text
  | AssetWithFullTitle Text Text
  | AssetWithId AssetId
  | AssetWithTrait Trait
  | AssetOwnedBy InvestigatorId
  | AssetMatches [AssetMatcher]
  | AssetReady
  | AssetWithUseType UseType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup AssetMatcher where
  AssetMatches xs <> AssetMatches ys = AssetMatches (xs <> ys)
  AssetMatches xs <> x = AssetMatches (x : xs)
  x <> AssetMatches xs = AssetMatches (x : xs)
  x <> y = AssetMatches [x, y]
