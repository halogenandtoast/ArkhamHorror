module Arkham.Location.Cards.LoftyWalkwayArchiveOfConflict (loftyWalkwayArchiveOfConflict) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (Discarded)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype LoftyWalkwayArchiveOfConflict = LoftyWalkwayArchiveOfConflict LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loftyWalkwayArchiveOfConflict :: LocationCard LoftyWalkwayArchiveOfConflict
loftyWalkwayArchiveOfConflict = location LoftyWalkwayArchiveOfConflict Cards.loftyWalkwayArchiveOfConflict 2 (Static 1)

instance HasAbilities LoftyWalkwayArchiveOfConflict where
  getAbilities (LoftyWalkwayArchiveOfConflict a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced
      $ Discarded #after (Just You) AnySource (basic IsEncounterCard)

instance RunMessage LoftyWalkwayArchiveOfConflict where
  runMessage msg l@(LoftyWalkwayArchiveOfConflict attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      enemies <- select $ NearestEnemyToLocation attrs.id (ReadyEnemy <> NonEliteEnemy)
      leadChooseOrRunOneM $ targets enemies \enemy -> do
        disengageFromAll enemy
        push $ EnemyMove enemy attrs.id
      pure l
    _ -> LoftyWalkwayArchiveOfConflict <$> liftRunMessage msg attrs
