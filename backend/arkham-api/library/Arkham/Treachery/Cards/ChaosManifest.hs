module Arkham.Treachery.Cards.ChaosManifest (chaosManifest) where

import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ChaosManifest = ChaosManifest TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosManifest :: TreacheryCard ChaosManifest
chaosManifest = treachery ChaosManifest Cards.chaosManifest

instance RunMessage ChaosManifest where
  runMessage msg t@(ChaosManifest attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      locations <- sampleLocations n
      chooseOrRunOneAtATimeM iid $ targets locations (`placeBreaches` 1)
      pure t
    _ -> ChaosManifest <$> liftRunMessage msg attrs
