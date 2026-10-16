module Arkham.Homebrew.AndThenThereWereNone.Enemies.RestlessSpirit_041 (
  restlessSpirit_041,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher

newtype RestlessSpirit_041 = RestlessSpirit_041 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Spawn - Edward Armstrong's or Vera Claythorne's location."
restlessSpirit_041 :: EnemyCard RestlessSpirit_041
restlessSpirit_041 =
  enemy RestlessSpirit_041 Cards.restlessSpirit_041
    & setSpawnAt (guestsLocation [Assets.edwardArmstrong, Assets.veraClaythorne])

{- | "__Forced__ - When Restless Spirit would take any amount of damage, if it is
ready: It attacks you."

@SourceUsedBy You@ is what makes the "you" in "It attacks you" a real
investigator: the window then belongs to whoever is dealing the damage, rather
than being offered to every seat. Damage from a source no investigator is using
has nobody to attack, which is the same answer the card gives.

The damage is not cancelled -- the attack is inserted and the hit still lands.
-}
instance HasAbilities RestlessSpirit_041 where
  getAbilities (RestlessSpirit_041 a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "restlessSpirit_041.lash"
      $ restricted a 1 (thisIs a ReadyEnemy)
      $ forced
      $ EnemyWouldTakeDamage #when (SourceUsedBy You) (be a)

instance RunMessage RestlessSpirit_041 where
  runMessage msg e@(RestlessSpirit_041 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> RestlessSpirit_041 <$> liftRunMessage msg attrs
