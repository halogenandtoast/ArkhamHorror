module Arkham.Location.Cards.LoftyWalkwayArchiveOfConflict (loftyWalkwayArchiveOfConflict) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype LoftyWalkwayArchiveOfConflict = LoftyWalkwayArchiveOfConflict LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loftyWalkwayArchiveOfConflict :: LocationCard LoftyWalkwayArchiveOfConflict
loftyWalkwayArchiveOfConflict = location LoftyWalkwayArchiveOfConflict Cards.loftyWalkwayArchiveOfConflict 2 (Static 1)

instance HasAbilities LoftyWalkwayArchiveOfConflict where
  getAbilities (LoftyWalkwayArchiveOfConflict a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (NearestEnemyToLocationFallback a.id $ ReadyEnemy <> NonEliteEnemy))
      $ forced
      $ DiscardedTopOfEncounterDeckBatch #after You AnySource

instance RunMessage LoftyWalkwayArchiveOfConflict where
  runMessage msg l@(LoftyWalkwayArchiveOfConflict attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      enemies <- select $ NearestEnemyToLocationFallback attrs.id $ ReadyEnemy <> NonEliteEnemy
      leadChooseOrRunOneM $ targets enemies \enemy -> do
        disengageFromAll enemy
        enemyMoveTo (attrs.ability 1) enemy attrs
      pure l
    _ -> LoftyWalkwayArchiveOfConflict <$> liftRunMessage msg attrs
