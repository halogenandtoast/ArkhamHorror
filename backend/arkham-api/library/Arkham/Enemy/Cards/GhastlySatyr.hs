module Arkham.Enemy.Cards.GhastlySatyr (ghastlySatyr) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Trait (Trait (Past, Present, Scientist))

newtype GhastlySatyr = GhastlySatyr EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghastlySatyr :: EnemyCard GhastlySatyr
ghastlySatyr = enemy GhastlySatyr Cards.ghastlySatyr (2, Static 3, 2) (1, 0)

instance HasModifiersFor GhastlySatyr where
  getModifiersFor (GhastlySatyr a) = do
    atPast <- selectAny $ locationWithEnemy a.id <> LocationWithTrait Past
    atPresent <- selectAny $ locationWithEnemy a.id <> LocationWithTrait Present
    modifySelf a $ [EnemyFight 1 | atPast] <> [EnemyFight 2 | atPresent]

instance HasAbilities GhastlySatyr where
  getAbilities (GhastlySatyr a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage GhastlySatyr where
  runMessage msg e@(GhastlySatyr attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> do
        scientists <- select $ AssetWithTrait Scientist <> AssetAt (LocationWithId lid)
        for_ scientists \scientist -> dealAssetDamage scientist (attrs.ability 1) 1
      pure e
    _ -> GhastlySatyr <$> liftRunMessage msg attrs
