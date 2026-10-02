module Arkham.Homebrew.CircusExMortis.Enemies.DarkYoungJuggernaut (darkYoungJuggernaut) where

import Arkham.Ability
import Arkham.Distance (unDistance)
import Arkham.Enemy.Import.Lifted
import Arkham.GameEnv (getDistance)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Query (getActiveInvestigatorId)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype DarkYoungJuggernaut = DarkYoungJuggernaut EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "__Spawn__ - Any location the same number of connections from you as Shub-Niggurath."

The def carries the wide half of that ("any location") and the narrowing happens on
'EnemySpawnAtLocationMatching', because the number is only knowable once the drawing
investigator is. That message, not 'EnemySpawn', is where a @SpawnAt@ matcher is turned
into the list of locations to choose from ('Helpers.Enemy.spawnAt'), so it is the last
point at which the matcher can still be narrowed. On the Hunt's @ForceSpawn@ keeps
working: a forced spawn replaces @spawnAt@ outright, so it never arrives as the printed
"anywhere" this matches on, and if the distance cannot be measured the spawn degrades to
"any location" instead of discarding the enemy.
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
    EnemySpawnAtLocationMatching miid Anywhere eid | eid == attrs.id -> do
      -- "you" is whoever drew it; the message carries them, and the engine itself falls
      -- back to the active investigator when it does not.
      iid <- maybe getActiveInvestigatorId pure miid
      mDistance <- runMaybeT do
        shub <- MaybeT $ selectOne $ LocationWithEnemy (enemyIs Cards.shubNiggurath)
        here <- MaybeT $ getLocationOf iid
        unDistance <$> MaybeT (getDistance here shub)
      let
        narrowed = case mDistance of
          Nothing -> Anywhere
          Just d -> LocationWithDistanceFrom d (locationWithInvestigator iid) Anywhere
      DarkYoungJuggernaut
        <$> liftRunMessage (EnemySpawnAtLocationMatching miid narrowed eid) attrs
    -- "Deal 1 direct damage to each asset you control." The window's Where is the
    -- investigator's own location, so every investigator standing where it arrived gets
    -- their own copy of the trigger, which is what "your location" means here.
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (assetControlledBy iid) \aid -> dealAssetDirectDamage aid (attrs.ability 1) 1
      pure e
    _ -> DarkYoungJuggernaut <$> liftRunMessage msg attrs
