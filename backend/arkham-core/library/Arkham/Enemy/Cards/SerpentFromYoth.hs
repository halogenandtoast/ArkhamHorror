module Arkham.Enemy.Cards.SerpentFromYoth
  ( serpentFromYoth
  , SerpentFromYoth(..)
  ) where

import Arkham.Prelude

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword qualified as Keyword
import Arkham.Target

newtype SerpentFromYoth = SerpentFromYoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

serpentFromYoth :: EnemyCard SerpentFromYoth
serpentFromYoth =
  enemy SerpentFromYoth Cards.serpentFromYoth (3, Static 5, 3) (1, 2)

instance HasModifiersFor SerpentFromYoth where
  getModifiersFor _ (EnemyTarget eid) (SerpentFromYoth a) | toId a == eid = do
    vengeance <- getVengeanceInVictoryDisplay
    pure
      $ toModifiers a
      $ [ AddKeyword Keyword.Retaliate | vengeance >= 1 ]
      <> [ AddKeyword Keyword.Hunter | vengeance >= 2 ]
      <> [ DamageTaken (-1) | vengeance >= 3 ]
  getModifiersFor _ _ _ = pure []


instance RunMessage SerpentFromYoth where
  runMessage msg (SerpentFromYoth attrs) =
    SerpentFromYoth <$> runMessage msg attrs
