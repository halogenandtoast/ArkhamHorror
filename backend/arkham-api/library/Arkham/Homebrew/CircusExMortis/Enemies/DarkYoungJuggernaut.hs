module Arkham.Homebrew.CircusExMortis.Enemies.DarkYoungJuggernaut (darkYoungJuggernaut) where

import Arkham.Ability
import Arkham.Distance (unDistance)
import Arkham.Enemy.Import.Lifted
import Arkham.GameEnv (getDistance)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype DarkYoungJuggernaut = DarkYoungJuggernaut EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "__Spawn__ - Any location the same number of connections from you as Shub-Niggurath."

The def carries the wide half of that ("any location") and the distance narrowing happens
on the spawn message below, because the number is only knowable once the drawing
investigator is. Doing it that way rather than in 'InvestigatorDrawEnemy' keeps On the
Hunt's @ForceSpawn@ working: a forced spawn replaces @spawnAt@ outright, so it never
reaches the branch that narrows, and if the narrowing is ever unable to measure a distance
the spawn degrades to the printed "any location" instead of discarding the enemy.
-}
darkYoungJuggernaut :: EnemyCard DarkYoungJuggernaut
darkYoungJuggernaut =
  enemyWith DarkYoungJuggernaut Cards.darkYoungJuggernaut (spawnAtL ?~ SpawnAt Anywhere)

instance HasAbilities DarkYoungJuggernaut where
  getAbilities (DarkYoungJuggernaut a) =
    extend1 a
      $ restricted a 1 (youExist $ HasMatchingAsset AnyAsset)
      $ forced
      $ EnemyMoves #after YourLocation (be a)

instance RunMessage DarkYoungJuggernaut where
  runMessage msg e@(DarkYoungJuggernaut attrs) = runQueueT $ case msg of
    EnemySpawn details
      | details.enemy == attrs.id
      , SpawnAt Anywhere <- details.spawnAt
      , Just iid <- details.investigator -> do
          mDistance <- runMaybeT do
            shub <- MaybeT $ selectOne $ LocationWithEnemy (enemyIs Cards.shubNiggurath)
            here <- MaybeT $ getLocationOf iid
            unDistance <$> MaybeT (getDistance here shub)
          let
            narrowed = case mDistance of
              Nothing -> SpawnAt Anywhere
              Just d -> SpawnAt $ LocationWithDistanceFrom d (locationWithInvestigator iid) Anywhere
          DarkYoungJuggernaut
            <$> liftRunMessage (EnemySpawn details {spawnDetailsSpawnAt = narrowed}) attrs
    -- "Deal 1 direct damage to each asset you control." The window's Where is the
    -- investigator's own location, so every investigator standing where it arrived gets
    -- their own copy of the trigger, which is what "your location" means here.
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (assetControlledBy iid) \aid -> dealAssetDirectDamage aid (attrs.ability 1) 1
      pure e
    _ -> DarkYoungJuggernaut <$> liftRunMessage msg attrs
