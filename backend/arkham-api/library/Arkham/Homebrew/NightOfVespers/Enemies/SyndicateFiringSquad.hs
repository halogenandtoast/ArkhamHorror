-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Enemies.SyndicateFiringSquad (syndicateFiringSquad) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.NightOfVespers.CardDefs.Enemies qualified as Cards

newtype SyndicateFiringSquad = SyndicateFiringSquad EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

syndicateFiringSquad :: EnemyCard SyndicateFiringSquad
syndicateFiringSquad = enemy SyndicateFiringSquad Cards.syndicateFiringSquad

instance RunMessage SyndicateFiringSquad where
  runMessage msg (SyndicateFiringSquad attrs) = SyndicateFiringSquad <$> runMessage msg attrs
