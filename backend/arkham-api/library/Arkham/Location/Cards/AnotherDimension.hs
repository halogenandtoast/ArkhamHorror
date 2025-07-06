module Arkham.Location.Cards.AnotherDimension (anotherDimension) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Window (locationLeavingPlay)
import Arkham.Location.Cards qualified as Cards (anotherDimension)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.Window qualified as Window

newtype AnotherDimension = AnotherDimension LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anotherDimension :: LocationCard AnotherDimension
anotherDimension = location AnotherDimension Cards.anotherDimension 6 (Static 0)

instance HasAbilities AnotherDimension where
  getAbilities (AnotherDimension attrs) =
    extendRevealed1 attrs
      $ uncancellable
      $ mkAbility attrs 1
      $ forced
      $ LocationLeavesPlay #when
      $ oneOf [LocationWithEnemy AnyEnemy, LocationWithInvestigator Anyone]

instance RunMessage AnotherDimension where
  runMessage msg l@(AnotherDimension attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (locationLeavingPlay -> lid) _ -> do
      selectEach (InvestigatorAt $ be lid) \iid -> do
        withBatchedTimings (Window.Moves iid (attrs.ability 1) (Just lid) attrs.id) do
          moveToEdit (attrs.ability 1) iid attrs uncancellableMove
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 (locationLeavingPlay -> lid) _) -> do
      selectEach (EnemyAt (be lid)) \eid -> enemyMoveTo (attrs.ability 1) eid attrs.id
      pure l
    _ -> AnotherDimension <$> liftRunMessage msg attrs
