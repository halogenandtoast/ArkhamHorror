module Arkham.Homebrew.AgesUnwound.Enemies.BlessedOfAforgomon (blessedOfAforgomon) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (
  ModifierType (FewerActions),
  interactAsOneOf,
  modifySelect,
 )
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Placement

newtype BlessedOfAforgomon = BlessedOfAforgomon EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "__Forced__ - When Blessed of Aforgomon would spawn at any location: Put
Blessed of Aforgomon into play next to the agenda deck instead."

That is the whole of its spawn instruction, so it is 'SpawnPlaced' rather than a
__Forced__ ability: the card never reaches a location, and expressing it as a
spawn means nothing has to cancel a spawn that has already happened.
-}
blessedOfAforgomon :: EnemyCard BlessedOfAforgomon
blessedOfAforgomon =
  enemyWith BlessedOfAforgomon Cards.blessedOfAforgomon
    $ spawnAtL
    ?~ SpawnPlaced NextToAgenda

{- | "You may fight or evade Blessed of Aforgomon as if it were at your location
(it is not engaged with you). /
While Blessed of Aforgomon is ready, each investigator must take one fewer action
during each of their turns."

'interactAsOneOf' is the engine's shape for "fight or evade it as if it were
here": it closes the ordinary routes and reopens them through the matcher, so the
enemy stays unengaged.
-}
instance HasModifiersFor BlessedOfAforgomon where
  getModifiersFor (BlessedOfAforgomon a) = do
    interactAsOneOf a (be a)
    unless a.exhausted $ modifySelect a Anyone [FewerActions 1]

instance RunMessage BlessedOfAforgomon where
  runMessage msg (BlessedOfAforgomon attrs) =
    BlessedOfAforgomon <$> runQueueT (liftRunMessage msg attrs)
