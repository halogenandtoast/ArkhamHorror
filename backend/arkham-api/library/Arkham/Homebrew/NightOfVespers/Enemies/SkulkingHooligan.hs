-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Enemies.SkulkingHooligan (skulkingHooligan) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.NightOfVespers.CardDefs.Enemies qualified as Cards

newtype SkulkingHooligan = SkulkingHooligan EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

skulkingHooligan :: EnemyCard SkulkingHooligan
skulkingHooligan = enemy SkulkingHooligan Cards.skulkingHooligan

instance RunMessage SkulkingHooligan where
  runMessage msg (SkulkingHooligan attrs) = SkulkingHooligan <$> runMessage msg attrs
