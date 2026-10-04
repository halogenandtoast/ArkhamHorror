module Arkham.Homebrew.AgainstTheWendigo.Enemies.BestialCreature (bestialCreature) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype BestialCreature = BestialCreature EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Prey - The investigator with the least damage on her or him, and her or his
Ally assets." The Ally half is not expressible as a prey matcher, so the printed
tie-break the engine can see -- least damage -- is what is used.
-}
bestialCreature :: EnemyCard BestialCreature
bestialCreature =
  enemy BestialCreature Cards.bestialCreature
    & setSpawnAt (LocationWithMostInvestigators Anywhere)
    & setPrey LowestRemainingHealth

instance HasModifiersFor BestialCreature where
  -- "The Bestial Creature gets +2 [per_investigator] health."
  getModifiersFor (BestialCreature a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier (2 * n)]

instance RunMessage BestialCreature where
  runMessage msg (BestialCreature attrs) = runQueueT $ case msg of
    -- "Revelation - Shuffle the discard pile into the encounter deck."
    EnemySpawn details | details.enemy == attrs.id -> do
      shuffleEncounterDiscardBackIn
      BestialCreature <$> liftRunMessage msg attrs
    _ -> BestialCreature <$> liftRunMessage msg attrs
