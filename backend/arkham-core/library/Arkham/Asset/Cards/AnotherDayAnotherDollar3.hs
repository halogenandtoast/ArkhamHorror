module Arkham.Asset.Cards.AnotherDayAnotherDollar3 (
  anotherDayAnotherDollar3,
  AnotherDayAnotherDollar3 (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype AnotherDayAnotherDollar3 = AnotherDayAnotherDollar3 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anotherDayAnotherDollar3 :: AssetCard AnotherDayAnotherDollar3
anotherDayAnotherDollar3 =
  asset AnotherDayAnotherDollar3 Cards.anotherDayAnotherDollar3

instance HasModifiersFor AnotherDayAnotherDollar3 where
  getModifiersFor (InvestigatorTarget iid) (AnotherDayAnotherDollar3 attrs) =
    pure
      $ toModifiersWith
        attrs
        setActiveDuringSetup
        [StartingResources 2 | controlledBy attrs iid]
  getModifiersFor _ _ = pure []

instance RunMessage AnotherDayAnotherDollar3 where
  runMessage msg (AnotherDayAnotherDollar3 attrs) = AnotherDayAnotherDollar3 <$> runMessage msg attrs
