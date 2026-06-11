module Arkham.Location.Cards.ChamberOfSecretsMysteriousPrison (chamberOfSecretsMysteriousPrison) where

import Arkham.Ability
import Arkham.Helpers.FlavorText
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype ChamberOfSecretsMysteriousPrison = ChamberOfSecretsMysteriousPrison LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfSecretsMysteriousPrison :: LocationCard ChamberOfSecretsMysteriousPrison
chamberOfSecretsMysteriousPrison = location ChamberOfSecretsMysteriousPrison Cards.chamberOfSecretsMysteriousPrison 3 (PerPlayer 3)

instance HasAbilities ChamberOfSecretsMysteriousPrison where
  getAbilities (ChamberOfSecretsMysteriousPrison a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage ChamberOfSecretsMysteriousPrison where
  runMessage msg l@(ChamberOfSecretsMysteriousPrison attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      flavor $ scope "chamberOfSecrets" $ p "poundFists"
      pure l
    _ -> ChamberOfSecretsMysteriousPrison <$> liftRunMessage msg attrs
