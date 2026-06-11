module Arkham.Location.Cards.ChamberOfSecretsBloodyPrison (chamberOfSecretsBloodyPrison) where

import Arkham.Ability
import Arkham.Helpers.FlavorText
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype ChamberOfSecretsBloodyPrison = ChamberOfSecretsBloodyPrison LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfSecretsBloodyPrison :: LocationCard ChamberOfSecretsBloodyPrison
chamberOfSecretsBloodyPrison = location ChamberOfSecretsBloodyPrison Cards.chamberOfSecretsBloodyPrison 3 (PerPlayer 3)

instance HasAbilities ChamberOfSecretsBloodyPrison where
  getAbilities (ChamberOfSecretsBloodyPrison a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage ChamberOfSecretsBloodyPrison where
  runMessage msg l@(ChamberOfSecretsBloodyPrison attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      flavor $ scope "chamberOfSecrets" $ p "poundFists"
      pure l
    _ -> ChamberOfSecretsBloodyPrison <$> liftRunMessage msg attrs
