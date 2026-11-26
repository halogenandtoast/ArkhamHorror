module Arkham.Enemy.Cards.TheForgottenAge.AgentsOfYig.SerpentFromYoth (serpentFromYoth) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Enemy.CardDefs.TheForgottenAge.AgentsOfYig qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype SerpentFromYoth = SerpentFromYoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

serpentFromYoth :: EnemyCard SerpentFromYoth
serpentFromYoth = enemy SerpentFromYoth Cards.serpentFromYoth

instance HasModifiersFor SerpentFromYoth where
  getModifiersFor (SerpentFromYoth a) = do
    vengeance <- getVengeanceInVictoryDisplay
    modifySelfWhen a (vengeance >= 1) [AddKeyword Keyword.Retaliate]
    modifySelfWhen a (vengeance >= 2) [AddKeyword Keyword.Hunter]
    modifySelfWhen a (vengeance >= 3) [DamageTakenFrom AttackDamageEffect (-1)]

instance RunMessage SerpentFromYoth where
  runMessage msg (SerpentFromYoth attrs) = SerpentFromYoth <$> runMessage msg attrs
