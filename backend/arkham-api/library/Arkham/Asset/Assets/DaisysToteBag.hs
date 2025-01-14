module Arkham.Asset.Assets.DaisysToteBag (daisysToteBag) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Script
import Arkham.Trait

newtype DaisysToteBag = DaisysToteBag AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBag :: AssetCard DaisysToteBag
daisysToteBag = asset DaisysToteBag Cards.daisysToteBag

instance RunMessage DaisysToteBag where
  runMessage = script $ additionalSlots #hand 2 $ holds $ only Tome
