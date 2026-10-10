module Arkham.Homebrew.AgesUnwound.Enemies.EternitysSentinel_193b (
  eternitysSentinel_193b,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Matcher

newtype EternitysSentinel_193b = EternitysSentinel_193b EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Eternity's Sentinel (He's Been Waiting For So Long)/, the reverse of the
@:ages-unwound:193@ printing of /Secrets Long Forgotten/.

"__Spawn__ - Furthest location from all investigators."
-}
eternitysSentinel_193b :: EnemyCard EternitysSentinel_193b
eternitysSentinel_193b =
  enemyWith EternitysSentinel_193b Cards.eternitysSentinel_193b
    $ spawnAtL
    ?~ SpawnAt (FarthestLocationFromAll Anywhere)

{- | "If /the investigators slew their strange observer,/ Secrets Long Forgotten
gets +2 fight, +1 damage and +1 horror."

(The card names itself by the location it is printed on the back of.) The
Scenario I record is fixed for the whole campaign, so this is a static
self-modifier.
-}
instance HasModifiersFor EternitysSentinel_193b where
  getModifiersFor (EternitysSentinel_193b a) = do
    slew <- getHasRecord TheInvestigatorsSlewTheirStrangeObserver
    when slew $ modifySelf a [EnemyFight 2, DamageDealt 1, HorrorDealt 1]

-- | "__Forced__ - At the start of the enemy phase: Place 1 doom on the current agenda."
instance HasAbilities EternitysSentinel_193b where
  getAbilities (EternitysSentinel_193b a) =
    extend1 a $ mkAbility a 1 $ forced $ PhaseBegins #when #enemy

instance RunMessage EternitysSentinel_193b where
  runMessage msg e@(EternitysSentinel_193b attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoomOnAgenda 1
      pure e
    _ -> EternitysSentinel_193b <$> liftRunMessage msg attrs
