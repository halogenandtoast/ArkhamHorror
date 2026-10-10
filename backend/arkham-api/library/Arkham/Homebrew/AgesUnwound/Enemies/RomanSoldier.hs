module Arkham.Homebrew.AgesUnwound.Enemies.RomanSoldier (romanSoldier) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards

{- | The stand-in /Determined General/ and /Roman Outpost/ mint from the top card
of an investigator's deck: "a Roman Soldier enemy with 3 fight, 1 health, 3
evade, 1 damage and the [[Humanoid]] trait". Every one of those values is on the
@:ages-unwound:900@ def, and the card prints nothing else, so this module exists
only to register a builder.

It is not optional. @Arkham.Enemy.lookupEnemy@ resolves a card code against
@allEnemies@, which @cards-discover@ builds from the behaviour modules; a def
with no builder falls through to the database custom-card path and then
@error@s. Without this module the first Roman Soldier spawned would crash the
game rather than fail quietly.

@HasAbilities@ is derived via the __newtype__ so the enemy keeps 'EnemyAttrs''
basic fight/evade/engage abilities -- the @anyclass@ default is @const []@ and
would leave it unfightable.
-}
newtype RomanSoldier = RomanSoldier EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

romanSoldier :: EnemyCard RomanSoldier
romanSoldier = enemy RomanSoldier Cards.romanSoldier

instance RunMessage RomanSoldier where
  runMessage msg (RomanSoldier attrs) = RomanSoldier <$> runMessage msg attrs
