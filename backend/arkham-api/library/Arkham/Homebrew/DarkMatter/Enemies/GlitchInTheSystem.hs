module Arkham.Homebrew.DarkMatter.Enemies.GlitchInTheSystem (glitchInTheSystem) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype GlitchInTheSystem = GlitchInTheSystem EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glitchInTheSystem :: EnemyCard GlitchInTheSystem
glitchInTheSystem =
  enemy GlitchInTheSystem Cards.glitchInTheSystem
    & setSpawnAt (LocationWithoutInvestigators <> LocationWithoutEnemies)

{- | "Forced - After Glitch in the System's location is switched with another
location: Place 1 doom on Glitch in the System."
-}
instance HasAbilities GlitchInTheSystem where
  getAbilities (GlitchInTheSystem a) =
    extend1 a $ mkAbility a 1 $ forced $ ScenarioEvent #after Nothing "switched"

instance RunMessage GlitchInTheSystem where
  runMessage msg e@(GlitchInTheSystem attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> GlitchInTheSystem <$> liftRunMessage msg attrs
