module Arkham.Homebrew.DarkMatter.Enemies.VirtualByakhee (virtualByakhee) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Movement (cancelEnemyMovement)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype VirtualByakhee = VirtualByakhee EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Spawn - Furthest location from you. Hunter."
virtualByakhee :: EnemyCard VirtualByakhee
virtualByakhee =
  enemy VirtualByakhee Cards.virtualByakhee
    & setSpawnAt (FarthestLocationFromYou Anywhere)

instance HasModifiersFor VirtualByakhee where
  getModifiersFor (VirtualByakhee a) = modifySelf a [AddKeyword Keyword.Hunter]

{- | "Forced - When Virtual Byakhee would move between two locations without
investigators: Switch those locations with each other instead."

@EnemyMove@ opens a batched @EnemyWouldMove@ window carrying the source, the
origin and the destination (see @Arkham.Enemy.Runner@), and every move —
including a hunter move, which reaches @EnemyMove@ via @HunterMove@ — passes
through it. That is the same window Barriers, Decoys, and Traps hooks to bounce
an enemy off a barrier, so the "instead" is modelled by cancelling the move's
batch and asking the scenario to switch the two locations.

A forced ability that cannot change the game state is ignored, so the switch
being impossible means the ability does not trigger at all and the byakhee moves
normally. During act 1 (Public School 187) "Locations cannot be switched with
each other", so the ability is gated off for the duration.
-}
instance HasAbilities VirtualByakhee where
  getAbilities (VirtualByakhee a) =
    extend1 a
      $ restricted a 1 (not_ $ ActExists (ActWithStep 1))
      $ forced
      $ EnemyWouldMove #when (be a) AnySource LocationWithoutInvestigators LocationWithoutInvestigators

getWouldMoveLocations :: [Window] -> Maybe (LocationId, LocationId)
getWouldMoveLocations = \case
  (windowType -> Window.EnemyWouldMove _ _ fromLid toLid) : _ -> Just (fromLid, toLid)
  _ : rest -> getWouldMoveLocations rest
  [] -> Nothing

instance RunMessage VirtualByakhee where
  runMessage msg e@(VirtualByakhee attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getWouldMoveLocations -> Just (from, dest)) _ -> do
      {- This is a replacement effect: the move never happens, the two locations
      trade grid positions instead. Cancel first so the doomed move is off the
      queue before anything reacts to the switch. -}
      cancelEnemyMovement attrs.id
      push $ ScenarioSpecific "switchLocations" (toJSON (from, dest))
      pure e
    _ -> VirtualByakhee <$> liftRunMessage msg attrs
