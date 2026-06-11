module Arkham.Location.Cards.ChamberOfSecretsEnshroudedPrison (chamberOfSecretsEnshroudedPrison) where

import Arkham.Ability
import Arkham.Helpers.FlavorText
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype ChamberOfSecretsEnshroudedPrison = ChamberOfSecretsEnshroudedPrison LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfSecretsEnshroudedPrison :: LocationCard ChamberOfSecretsEnshroudedPrison
chamberOfSecretsEnshroudedPrison = location ChamberOfSecretsEnshroudedPrison Cards.chamberOfSecretsEnshroudedPrison 3 (PerPlayer 3)

instance HasAbilities ChamberOfSecretsEnshroudedPrison where
  getAbilities (ChamberOfSecretsEnshroudedPrison a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage ChamberOfSecretsEnshroudedPrison where
  runMessage msg l@(ChamberOfSecretsEnshroudedPrison attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      flavor $ scope "chamberOfSecrets" $ p "poundFists"
      pure l
    _ -> ChamberOfSecretsEnshroudedPrison <$> liftRunMessage msg attrs
