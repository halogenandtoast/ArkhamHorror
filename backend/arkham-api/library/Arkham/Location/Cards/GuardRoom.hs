module Arkham.Location.Cards.GuardRoom (guardRoom) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype GuardRoom = GuardRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardRoom :: LocationCard GuardRoom
guardRoom = symbolLabel $ location GuardRoom Cards.guardRoom 2 (PerPlayer 2)

instance HasAbilities GuardRoom where
  getAbilities (GuardRoom a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> not_ (exists $ EnemyAt (be a))
            <> thisExists a LocationWithoutClues
            <> Remembered ObtainedAnEmployeeUniform
        )
        actionAbility

instance RunMessage GuardRoom where
  runMessage msg l@(GuardRoom attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember ImpersonatedAGuard
      pure l
    _ -> GuardRoom <$> liftRunMessage msg attrs
