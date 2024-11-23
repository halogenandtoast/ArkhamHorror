module Arkham.Enemy.Cards.ManifestationOfMadness (manifestationOfMadness, ManifestationOfMadness (..)) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Capability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Modifier

newtype ManifestationOfMadness = ManifestationOfMadness EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manifestationOfMadness :: EnemyCard ManifestationOfMadness
manifestationOfMadness = enemy ManifestationOfMadness Cards.manifestationOfMadness (3, Static 3, 3) (1, 1)

instance HasAbilities ManifestationOfMadness where
  getAbilities (ManifestationOfMadness a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #when You AnyEnemyAttack (be a)

instance RunMessage ManifestationOfMadness where
  runMessage msg e@(ManifestationOfMadness attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canShuffle <- can.manipulate.deck iid
      if canShuffle
        then do
          cards <- getTekelili 2
          when (notNull cards) $ addTekelili iid cards
          let x = max 0 (2 - length cards)
          when (x > 0) $ enemyAttackModifiers (attrs.ability 1) attrs [DamageDealt x, HorrorDealt x]
        else enemyAttackModifiers (attrs.ability 1) attrs [DamageDealt 2, HorrorDealt 2]
      pure e
    _ -> ManifestationOfMadness <$> liftRunMessage msg attrs
