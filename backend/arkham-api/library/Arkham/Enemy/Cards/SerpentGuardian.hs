module Arkham.Enemy.Cards.SerpentGuardian (serpentGuardian) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelfWhen)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype SerpentGuardian = SerpentGuardian EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

serpentGuardian :: EnemyCard SerpentGuardian
serpentGuardian =
  enemy SerpentGuardian Cards.serpentGuardian (4, Static 5, 1) (2, 0)
    & setSpawnAt (NearestLocationToYou LocationWithAnyClues)

instance HasModifiersFor SerpentGuardian where
  getModifiersFor (SerpentGuardian a) = do
    whenMatch a.id UnengagedEnemy $ modifySelect a (locationWithEnemy a) [ShroudModifier 2]
    n <- getVengeanceInVictoryDisplay
    modifySelfWhen a (n >= 3) [RemoveKeyword Keyword.Aloof, AddKeyword Keyword.Hunter]

instance RunMessage SerpentGuardian where
  runMessage msg (SerpentGuardian attrs) = SerpentGuardian <$> runMessage msg attrs
