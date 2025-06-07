module Arkham.Enemy.Cards.SerpentFromYoth (serpentFromYoth) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype SerpentFromYoth = SerpentFromYoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

serpentFromYoth :: EnemyCard SerpentFromYoth
serpentFromYoth = enemy SerpentFromYoth Cards.serpentFromYoth (3, Static 5, 3) (1, 2)

instance HasModifiersFor SerpentFromYoth where
  getModifiersFor (SerpentFromYoth a) = do
    vengeance <- getVengeanceInVictoryDisplay
    modifySelf a
      $ [AddKeyword Keyword.Retaliate | vengeance >= 1]
      <> [AddKeyword Keyword.Hunter | vengeance >= 2]
      <> [DamageTakenFrom AttackDamageEffect (-1) | vengeance >= 3]

instance RunMessage SerpentFromYoth where
  runMessage msg (SerpentFromYoth attrs) = SerpentFromYoth <$> runMessage msg attrs
