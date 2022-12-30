module Arkham.Asset.Cards.DetectivesColt1911s
  ( detectivesColt1911s
  , DetectivesColt1911s(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Asset.Runner

newtype DetectivesColt1911s = DetectivesColt1911s AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detectivesColt1911s :: AssetCard DetectivesColt1911s
detectivesColt1911s =
  asset DetectivesColt1911s Cards.detectivesColt1911s

instance RunMessage DetectivesColt1911s where
  runMessage msg (DetectivesColt1911s attrs) = DetectivesColt1911s <$> runMessage msg attrs
