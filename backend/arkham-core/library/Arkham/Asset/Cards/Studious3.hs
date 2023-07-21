module Arkham.Asset.Cards.Studious3 (
  studious3,
  Studious3 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype Studious3 = Studious3 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studious3 :: AssetCard Studious3
studious3 = asset Studious3 Cards.studious3

instance HasModifiersFor Studious3 where
  getModifiersFor (InvestigatorTarget iid) (Studious3 a) =
    pure $ toModifiersWith a setActiveDuringSetup [StartingHand 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance RunMessage Studious3 where
  runMessage msg (Studious3 attrs) = Studious3 <$> runMessage msg attrs
