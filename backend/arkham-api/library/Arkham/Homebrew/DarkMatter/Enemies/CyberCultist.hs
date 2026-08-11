module Arkham.Homebrew.DarkMatter.Enemies.CyberCultist (cyberCultist) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (ScanResult (..), scanEvent)
import Arkham.Homebrew.DarkMatter.Traits (pattern Colony)
import Arkham.Matcher
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype CyberCultist = CyberCultist EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cyberCultist :: EnemyCard CyberCultist
cyberCultist = enemy CyberCultist Cards.cyberCultist & setSpawnAt (LocationWithTrait Colony)

-- | "Forced - After you perform a successful scan: Place 1 doom on Cyber-Cultist."
instance HasAbilities CyberCultist where
  getAbilities (CyberCultist a) =
    extend1 a $ mkAbility a 1 $ forced $ ScenarioEvent #after Nothing scanEvent

getScanResult :: [Window] -> Maybe ScanResult
getScanResult = \case
  (windowType -> Window.ScenarioEvent key _ v) : _ | key == scanEvent -> Just (toResult v)
  _ : rest -> getScanResult rest
  [] -> Nothing

instance RunMessage CyberCultist where
  runMessage msg e@(CyberCultist attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getScanResult -> Just r) _ | scanSuccessful r -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> CyberCultist <$> liftRunMessage msg attrs
