module Arkham.Homebrew.CircusExMortis.Enemies.TheCultEnMasseRingmastersFervor (
  theCultEnMasseRingmastersFervor,
) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Enemies.TheCultEnMasseLeaderlessFanaticism (
  cultEnMasseModifiers,
 )

newtype TheCultEnMasseRingmastersFervor = TheCultEnMasseRingmastersFervor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCultEnMasseRingmastersFervor :: EnemyCard TheCultEnMasseRingmastersFervor
theCultEnMasseRingmastersFervor =
  enemy TheCultEnMasseRingmastersFervor Cards.theCultEnMasseRingmastersFervor

instance HasModifiersFor TheCultEnMasseRingmastersFervor where
  getModifiersFor (TheCultEnMasseRingmastersFervor a) = cultEnMasseModifiers a

instance RunMessage TheCultEnMasseRingmastersFervor where
  runMessage msg (TheCultEnMasseRingmastersFervor attrs) =
    runQueueT $ TheCultEnMasseRingmastersFervor <$> liftRunMessage msg attrs
