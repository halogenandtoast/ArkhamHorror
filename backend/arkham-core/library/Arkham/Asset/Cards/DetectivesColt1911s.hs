module Arkham.Asset.Cards.DetectivesColt1911s
  ( detectivesColt1911s
  , DetectivesColt1911s(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import qualified Arkham.Asset.Cards as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Target
import Arkham.Trait (Trait(Tool))

newtype DetectivesColt1911s = DetectivesColt1911s AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detectivesColt1911s :: AssetCard DetectivesColt1911s
detectivesColt1911s = asset DetectivesColt1911s Cards.detectivesColt1911s

instance HasModifiersFor DetectivesColt1911s where
  getModifiersFor (AssetTarget aid) (DetectivesColt1911s a) = do
    case assetController a of
      Nothing -> pure []
      Just iid -> do
        toolAssetsWithHands <- selectList
          $ assetControlledBy iid
          <> AssetWithTrait Tool
          <> AssetInSlot HandSlot
        pure $ toModifiers
          a
          [ DoNotTakeUpSlot HandSlot | aid `elem` take 2 toolAssetsWithHands ]
  getModifiersFor _ _ = pure []

instance HasAbilities DetectivesColt1911s where
  getAbilities (DetectivesColt1911s a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage DetectivesColt1911s where
  runMessage msg (DetectivesColt1911s attrs) =
    DetectivesColt1911s <$> runMessage msg attrs
