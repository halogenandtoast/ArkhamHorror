module Arkham.Types.AssetMatcher where

import Arkham.Prelude

import Arkham.Types.Id

import Arkham.Types.Asset.Uses
import Arkham.Types.Card.CardDef
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait

data AssetMatcher
  = AssetWithTitle Text
  | AssetWithFullTitle Text Text
  | AssetWithId AssetId
  | AssetWithClass ClassSymbol
  | AssetWithTrait Trait
  | AssetOwnedBy InvestigatorId
  | AssetMatches [AssetMatcher]
  | AssetOneOf [AssetMatcher]
  | AssetAtLocation LocationId
  | AssetNonStory
  | AssetReady
  | AssetWithUseType UseType
  | AssetIs CardDef
  | AnyAsset
  | EnemyAsset EnemyId
  | LocationAsset LocationId
  | DiscardableAsset
  | AssetCanBeAssignedDamageBy InvestigatorId
  | AssetCanBeAssignedHorrorBy InvestigatorId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup AssetMatcher where
  AnyAsset <> x = x
  x <> AnyAsset = x
  AssetMatches xs <> AssetMatches ys = AssetMatches (xs <> ys)
  AssetMatches xs <> x = AssetMatches (x : xs)
  x <> AssetMatches xs = AssetMatches (x : xs)
  x <> y = AssetMatches [x, y]

instance Monoid AssetMatcher where
  mempty = AnyAsset
