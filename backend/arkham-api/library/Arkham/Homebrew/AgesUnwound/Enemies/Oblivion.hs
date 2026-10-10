module Arkham.Homebrew.AgesUnwound.Enemies.Oblivion (oblivion) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Projection

newtype Oblivion = Oblivion EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Oblivion (Destruction Given Form)/, the reverse of the @:ages-unwound:204@
printing of /The End of All Things/. "__Spawn__ - The Future. __Hunter__.
__Massive__."

It prints no health at all -- @*@, with "The End of All Things cannot be defeated"
underneath. 'healthStar' is left on the card def so the browser shows the @*@, but
a 'Arkham.GameValue.ValueStar' health evaluates to @0@ and @CheckDefeated@
compares @damage >= health@, so the attrs' health is cleared here: with
@EnemyHealth@ 'Nothing' the defeat check is skipped entirely, which is exactly
"cannot be defeated". See
@mcp/references/engine-gotchas/project_healthstar_enemy_defeats_itself_immediately.md@.
-}
oblivion :: EnemyCard Oblivion
oblivion =
  enemyWith
    Oblivion
    Cards.oblivion
    ((healthL .~ Nothing) . (spawnAtL ?~ SpawnAt (locationIs Locations.theFuture)))

{- | "For each 1[per_investigator] damage on The End of All Things, it gets -1
damage value and -1 horror value."

(The card names itself by the location it is printed on the back of.) Damage is
the only way to blunt it, since it cannot be defeated -- which is why the
start-of-phase __Forced__ heals it back.
-}
instance HasModifiersFor Oblivion where
  getModifiersFor (Oblivion a) = do
    perInvestigator <- perPlayer 1
    damage <- field EnemyDamage a.id
    let steps = damage `div` max 1 perInvestigator
    when (steps > 0) $ modifySelf a [DamageDealt (-steps), HorrorDealt (-steps)]

{- | "__Forced__ - At the start of the enemy phase: If The End of All Things is
ready, it heals 1[per_investigator] damage. Otherwise, ready The End of All
Things."
-}
instance HasAbilities Oblivion where
  getAbilities (Oblivion a) =
    extend1 a $ mkAbility a 1 $ forced $ PhaseBegins #when #enemy

instance RunMessage Oblivion where
  runMessage msg e@(Oblivion attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      if attrs.exhausted
        then readyThis attrs
        else do
          n <- perPlayer 1
          healDamage attrs (attrs.ability 1) n
      pure e
    _ -> Oblivion <$> liftRunMessage msg attrs
