module Arkham.Enemy.Cards.DrWentworthMoore (drWentworthMoore) where

import Arkham.Ability
import Arkham.Card (toCard)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhenM)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Trait (Trait (Monster))

newtype DrWentworthMoore = DrWentworthMoore EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drWentworthMoore :: EnemyCard DrWentworthMoore
drWentworthMoore =
  enemyWith DrWentworthMoore Cards.drWentworthMoore (3, Static 1, 3) (1, 1)
    $ spawnAtL
    ?~ SpawnAt "Museum of Egyptian Antiquities"

instance HasModifiersFor DrWentworthMoore where
  getModifiersFor (DrWentworthMoore a) = do
    modifySelfWhenM
      a
      (selectAny $ ReadyEnemy <> EnemyWithTrait Monster <> EnemyAt (locationWithEnemy a))
      [CannotBeDamaged]

instance HasAbilities DrWentworthMoore where
  getAbilities (DrWentworthMoore a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEntersPlay #after (be a)

instance RunMessage DrWentworthMoore where
  runMessage msg e@(DrWentworthMoore attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      findEncounterCard lead attrs (#enemy <> CardWithTrait Monster)
      pure e
    FoundEncounterCard _ (isTarget attrs -> True) (toCard -> card) -> do
      withLocationOf attrs \loc -> spawnEnemyAt_ card loc
      pure e
    _ -> DrWentworthMoore <$> liftRunMessage msg attrs
