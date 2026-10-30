-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Enemies.HiredTriggermen (hiredTriggermen) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.NightOfVespers.CardDefs.Enemies qualified as Cards

newtype HiredTriggermen = HiredTriggermen EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hiredTriggermen :: EnemyCard HiredTriggermen
hiredTriggermen = enemy HiredTriggermen Cards.hiredTriggermen

instance RunMessage HiredTriggermen where
  runMessage msg (HiredTriggermen attrs) = HiredTriggermen <$> runMessage msg attrs
