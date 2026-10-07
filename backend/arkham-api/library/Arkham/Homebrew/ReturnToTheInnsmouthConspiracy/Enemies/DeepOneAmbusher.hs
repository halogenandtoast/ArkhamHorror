module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Enemies.DeepOneAmbusher (deepOneAmbusher) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (deepOneInvestigator)
import Arkham.Matcher

{- | "Prey - Deep One investigators." / "Spawn - Engaged with Prey, if possible."
The "if possible" is the second branch: with no Deep One investigator in play it
spawns at the drawing investigator's location as usual.
-}
newtype DeepOneAmbusher = DeepOneAmbusher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneAmbusher :: EnemyCard DeepOneAmbusher
deepOneAmbusher =
  enemy DeepOneAmbusher Cards.deepOneAmbusher
    & setSpawnAtFirst [SpawnEngagedWith deepOneInvestigator, SpawnAt YourLocation]
    & setPrey deepOneInvestigator

instance HasAbilities DeepOneAmbusher where
  getAbilities (DeepOneAmbusher a) = extend1 a $ forcedAbility a 1 $ EnemyEngaged #after You (be a)

instance RunMessage DeepOneAmbusher where
  runMessage msg e@(DeepOneAmbusher attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid attrs 1
      pure e
    _ -> DeepOneAmbusher <$> liftRunMessage msg attrs
