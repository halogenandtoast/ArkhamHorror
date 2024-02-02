module Arkham.Location.Cards.BurnedRuins_204 (
  burnedRuins_204,
  BurnedRuins_204 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (burnedRuins_204)
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype BurnedRuins_204 = BurnedRuins_204 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

burnedRuins_204 :: LocationCard BurnedRuins_204
burnedRuins_204 = location BurnedRuins_204 Cards.burnedRuins_204 3 (Static 3)

instance HasModifiersFor BurnedRuins_204 where
  getModifiersFor (EnemyTarget eid) (BurnedRuins_204 attrs) = do
    atLocation <- enemyAtLocation eid attrs
    pure $ toModifiers attrs [EnemyEvade 1 | atLocation]
  getModifiersFor _ _ = pure []

instance HasAbilities BurnedRuins_204 where
  getAbilities = withDrawCardUnderneathAction

instance RunMessage BurnedRuins_204 where
  runMessage msg (BurnedRuins_204 attrs) =
    BurnedRuins_204 <$> runMessage msg attrs
