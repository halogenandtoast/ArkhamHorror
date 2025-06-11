module Arkham.Asset.Assets.ClintonFreemanShouldHaveStayedHome (clintonFreemanShouldHaveStayedHome) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ClintonFreemanShouldHaveStayedHome = ClintonFreemanShouldHaveStayedHome AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clintonFreemanShouldHaveStayedHome :: AssetCard ClintonFreemanShouldHaveStayedHome
clintonFreemanShouldHaveStayedHome = asset ClintonFreemanShouldHaveStayedHome Cards.clintonFreemanShouldHaveStayedHome

instance RunMessage ClintonFreemanShouldHaveStayedHome where
  runMessage msg (ClintonFreemanShouldHaveStayedHome attrs) = runQueueT $ case msg of
    _ -> ClintonFreemanShouldHaveStayedHome <$> liftRunMessage msg attrs
