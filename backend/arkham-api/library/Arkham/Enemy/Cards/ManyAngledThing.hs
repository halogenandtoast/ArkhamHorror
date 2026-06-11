module Arkham.Enemy.Cards.ManyAngledThing (manyAngledThing) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Future, Past, Present))

newtype ManyAngledThing = ManyAngledThing EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manyAngledThing :: EnemyCard ManyAngledThing
manyAngledThing =
  enemy ManyAngledThing Cards.manyAngledThing (2, Static 7, 2) (1, 1)
    & setSpawnAt (locationIs Locations.tindalos)

instance HasModifiersFor ManyAngledThing where
  getModifiersFor (ManyAngledThing a) = do
    atPast <- selectAny $ locationWithEnemy a.id <> LocationWithTrait Past
    atPresent <- selectAny $ locationWithEnemy a.id <> LocationWithTrait Present
    atFuture <- selectAny $ locationWithEnemy a.id <> LocationWithTrait Future
    modifySelf a
      $ (guard atPast *> [EnemyEvade 1, AddKeyword Keyword.Alert])
      <> (guard atPresent *> [DamageDealt 1, HorrorDealt 1])
      <> (guard atFuture *> [EnemyFight 1, AddKeyword Keyword.Retaliate])

instance RunMessage ManyAngledThing where
  runMessage msg (ManyAngledThing attrs) = ManyAngledThing <$> runMessage msg attrs
