module Arkham.Location.Cards.AnotherDimension (anotherDimension, AnotherDimension (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Window (locationLeavingPlay)
import Arkham.Location.Cards qualified as Cards (anotherDimension)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.Movement qualified as Move
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
        withTimings (Window.Moves iid (attrs.ability 1) (Just lid) attrs.id) do
          moveTo_ (attrs.ability 1) iid $ uncancellableMove $ Move.move (attrs.ability 1) iid attrs.id
      selectEach (UnengagedEnemy <> at_ (be lid)) \eid -> push $ EnemyMove eid attrs.id
      pure l
    _ -> AnotherDimension <$> liftRunMessage msg attrs
