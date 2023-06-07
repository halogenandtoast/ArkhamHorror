module Arkham.Asset.Cards.PuzzleBox
  ( puzzleBox
  , PuzzleBox(..)
  )
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype PuzzleBox = PuzzleBox AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

puzzleBox :: AssetCard PuzzleBox
puzzleBox =
  asset PuzzleBox Cards.puzzleBox

instance RunMessage PuzzleBox where
  runMessage msg (PuzzleBox attrs) = PuzzleBox <$> runMessage msg attrs
