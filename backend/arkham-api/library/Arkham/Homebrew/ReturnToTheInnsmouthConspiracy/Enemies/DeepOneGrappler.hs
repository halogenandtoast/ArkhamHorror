module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Enemies.DeepOneGrappler (deepOneGrappler) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Move (enemyMoveTo, moveTo)

newtype DeepOneGrappler = DeepOneGrappler EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving anyclass IsEnemy

{- | "Spawn - Nearest fully flooded location. If there are no such locations in play,
Deep One Grappler gains surge."
-}
deepOneGrappler :: EnemyCard DeepOneGrappler
deepOneGrappler =
  enemyWith DeepOneGrappler Cards.deepOneGrappler
    $ (spawnAtL ?~ SpawnAt (NearestLocationToYou FullyFloodedLocation))
    . (surgeIfUnableToSpawnL .~ True)

{- | "While not engaged, Deep One Grappler can only enter fully flooded locations. While
moving, Deep One Grappler treats all fully flooded locations as if they were connected."
-}
instance HasModifiersFor DeepOneGrappler where
  getModifiersFor (DeepOneGrappler a) = do
    unengaged <- selectNone $ investigatorEngagedWith a
    when unengaged do
      -- 'CannotEnter' names a single location, so the restriction is spelled out as one
      -- modifier per location that is not fully flooded.
      unflooded <- select $ not_ FullyFloodedLocation
      modifySelf a $ MovesAsIfConnectedTo FullyFloodedLocation : map CannotEnter unflooded

{- | "Forced - After Deep One Grappler engages you: Move to the location directly below
yours." Nothing to do from the bottom row, so it does not ask.
-}
instance HasAbilities DeepOneGrappler where
  getAbilities (DeepOneGrappler a) =
    extend1 a
      $ restricted a 1 (exists $ LocationInDirection Below YourLocation)
      $ forced
      $ EnemyEngaged #after You (be a)

instance RunMessage DeepOneGrappler where
  runMessage msg e@(DeepOneGrappler attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      below <- select $ LocationInDirection Below (locationWithInvestigator iid)
      for_ (take 1 below) \lid -> do
        moveTo (attrs.ability 1) iid lid
        enemyMoveTo (attrs.ability 1) attrs lid
      pure e
    _ -> DeepOneGrappler <$> liftRunMessage msg attrs
