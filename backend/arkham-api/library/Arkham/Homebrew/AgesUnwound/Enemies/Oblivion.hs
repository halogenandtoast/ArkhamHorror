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

oblivion :: EnemyCard Oblivion
oblivion =
  enemyWith Oblivion Cards.oblivion
    $ (healthL .~ Nothing)
    . (spawnAtL ?~ SpawnAt (locationIs Locations.theFuture))

instance HasModifiersFor Oblivion where
  getModifiersFor (Oblivion a) = do
    perInvestigator <- perPlayer 1
    damage <- field EnemyDamage a.id
    let steps = damage `div` max 1 perInvestigator
    when (steps > 0) $ modifySelf a [DamageDealt (-steps), HorrorDealt (-steps)]

instance HasAbilities Oblivion where
  getAbilities (Oblivion a) =
    extend1 a
      $ restricted a 1 (thisIs a $ oneOf [#exhausted, EnemyWithAnyDamage])
      $ forced
      $ PhaseBegins #when #enemy

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
