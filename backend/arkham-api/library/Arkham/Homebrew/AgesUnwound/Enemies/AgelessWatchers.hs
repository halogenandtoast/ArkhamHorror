module Arkham.Homebrew.AgesUnwound.Enemies.AgelessWatchers (agelessWatchers) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (PlayCard)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype AgelessWatchers = AgelessWatchers EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "__Spawn__ - Furthest empty location. (If no locations are empty, Ageless
Watchers gains surge.)"

__Hunter__ and __Swarming 1__ are printed keywords and come off the card def. The
parenthetical is 'surgeIfUnableToSpawnL': the engine's own unable-to-spawn path
discards the card and, with that flag set, surges -- which is the printed
fallback and the Rules Reference's default for a spawn instruction that cannot be
followed, in one place.
-}
agelessWatchers :: EnemyCard AgelessWatchers
agelessWatchers =
  enemyWith AgelessWatchers Cards.agelessWatchers
    $ (spawnAtL ?~ SpawnAt (FarthestLocationFromAll EmptyLocation))
    . (surgeIfUnableToSpawnL .~ True)

{- | "__Forced__ - After an event is played at any location: Place that card
beneath Ageless Watchers, as a swarm card."

"At any location" is why the window is not narrowed to its own: a swarm card can
come from anywhere on the table. 'PlacedSwarmCard' is the message the swarm
machinery listens for; the card has to be obtained first so it stops counting as
being in its owner's discard pile.

The window is 'PlayCard' rather than 'PlayEvent' because the latter carries only
an 'Arkham.Id.EventId' -- and by the time the @#after@ window resolves the event
entity may already be gone -- while 'PlayCard' carries the card itself.
-}
instance HasAbilities AgelessWatchers where
  getAbilities (AgelessWatchers a) =
    extend1 a $ mkAbility a 1 $ forced $ PlayCard #after Anyone (basic #event)

instance RunMessage AgelessWatchers where
  runMessage msg e@(AgelessWatchers attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      for_ [cp.card | (windowType -> Window.PlayCard _ cp) <- ws] \card -> do
        obtainCard card
        push $ PlacedSwarmCard attrs.id card
      pure e
    _ -> AgelessWatchers <$> liftRunMessage msg attrs
