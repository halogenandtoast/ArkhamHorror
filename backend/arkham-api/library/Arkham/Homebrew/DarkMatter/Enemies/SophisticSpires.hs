module Arkham.Homebrew.DarkMatter.Enemies.SophisticSpires (sophisticSpires) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype SophisticSpires = SophisticSpires EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- | "Spawn - Nearest [[Carcosa]] location."
sophisticSpires :: EnemyCard SophisticSpires
sophisticSpires =
  enemy SophisticSpires Cards.sophisticSpires
    & setSpawnAt (NearestLocationToYou $ LocationWithTrait Carcosa)

-- | "Massive. / Does not attack during the enemy phase."
instance HasModifiersFor SophisticSpires where
  getModifiersFor (SophisticSpires a) =
    modifySelf a [AddKeyword Keyword.Massive, CannotAttackDuringEnemyPhase]

instance RunMessage SophisticSpires where
  runMessage msg (SophisticSpires attrs) = SophisticSpires <$> runMessage msg attrs
