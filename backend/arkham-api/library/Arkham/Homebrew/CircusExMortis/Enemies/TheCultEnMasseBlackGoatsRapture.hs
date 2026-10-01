module Arkham.Homebrew.CircusExMortis.Enemies.TheCultEnMasseBlackGoatsRapture (
  theCultEnMasseBlackGoatsRapture,
) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Enemies.TheCultEnMasseLeaderlessFanaticism (
  cultEnMasseModifiers,
 )

newtype TheCultEnMasseBlackGoatsRapture = TheCultEnMasseBlackGoatsRapture EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCultEnMasseBlackGoatsRapture :: EnemyCard TheCultEnMasseBlackGoatsRapture
theCultEnMasseBlackGoatsRapture =
  enemy TheCultEnMasseBlackGoatsRapture Cards.theCultEnMasseBlackGoatsRapture

instance HasModifiersFor TheCultEnMasseBlackGoatsRapture where
  getModifiersFor (TheCultEnMasseBlackGoatsRapture a) = cultEnMasseModifiers a

instance RunMessage TheCultEnMasseBlackGoatsRapture where
  runMessage msg (TheCultEnMasseBlackGoatsRapture attrs) =
    runQueueT $ TheCultEnMasseBlackGoatsRapture <$> liftRunMessage msg attrs
