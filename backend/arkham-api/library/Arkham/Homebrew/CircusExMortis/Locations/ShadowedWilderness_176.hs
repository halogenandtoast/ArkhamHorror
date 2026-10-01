module Arkham.Homebrew.CircusExMortis.Locations.ShadowedWilderness_176 (
  shadowedWilderness_176,
) where

import Arkham.Ability
import Arkham.Helpers.Window (enteringEnemy)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype ShadowedWilderness_176 = ShadowedWilderness_176 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowedWilderness_176 :: LocationCard ShadowedWilderness_176
shadowedWilderness_176 = location ShadowedWilderness_176 Cards.shadowedWilderness_176 4 (PerPlayer 1)

instance HasAbilities ShadowedWilderness_176 where
  getAbilities (ShadowedWilderness_176 a) =
    extendRevealed1 a
      {- A special case for this card alone, not a general rule about "after ... enters".
      The card prints "[reaction] After a non-Elite enemy enters this location", but the
      designer was asked whether an enemy that gains aloof this way still ends up engaged
      with an investigator already standing there, and answered: "the intention is that
      they gain aloof before engaging anyone" (The Beard, 2026-10-01).

      So the window is #when, which is the only anchor ahead of the arrival engagement
      check -- `After (EnemyEntered)` runs `EnemyCheckEngagement` before the #after window
      (Enemy/Runner.hs, which says so in its own comment). Nothing else changes: other
      "after ... enters" reactions in this campaign, Crowded Row (:049) among them, stay
      #after, and the engine's ordering is untouched.

      Two things that look wrong and are not. The count stays at 1, because `EnemyEntered`
      writes `AtLocation` in its own handler body and the entity is committed before
      anything it pushed drains -- the entering enemy is already here when #when resolves.
      And a disengage would not be equivalent: it opens `Window.EnemyEngaged` first, so
      reactions to engagement would fire on an enemy the ruling says never engaged.

      Spawning is deliberately not covered: the card says "enters", and on the spawn path
      engagement resolves before any enters window is raised at all. -}
      $ restricted a 1 (EnemyCount (EqualTo $ Static 1) (enemyAt a))
      $ freeReaction (EnemyEnters #when (be a) (NonEliteEnemy <> not_ AloofEnemy))

instance RunMessage ShadowedWilderness_176 where
  runMessage msg l@(ShadowedWilderness_176 attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (enteringEnemy -> enemy) _ -> do
      roundModifier (attrs.ability 1) enemy (AddKeyword Keyword.Aloof)
      pure l
    _ -> ShadowedWilderness_176 <$> liftRunMessage msg attrs
