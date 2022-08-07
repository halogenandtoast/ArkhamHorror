module Arkham.Enemy.Cards.BroodOfYig
  ( broodOfYig
  , BroodOfYig(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Enemy.Runner hiding (EnemyFight)
import Arkham.Helpers.Modifiers
import Arkham.Target

newtype BroodOfYig = BroodOfYig EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodOfYig :: EnemyCard BroodOfYig
broodOfYig = enemy BroodOfYig Cards.broodOfYig (2, Static 3, 2) (1, 1)

instance HasModifiersFor BroodOfYig where
  getModifiersFor (EnemyTarget eid) (BroodOfYig a) | toId a == eid
    = do
      vengeance <- getVengeanceInVictoryDisplay
      pure $ toModifiers a [EnemyFight vengeance | vengeance > 0]
  getModifiersFor _ _ = pure []

instance RunMessage BroodOfYig where
  runMessage msg (BroodOfYig attrs) =
    BroodOfYig <$> runMessage msg attrs
