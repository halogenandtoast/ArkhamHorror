module Arkham.Act.Cards.AscendingTheHillV1 (ascendingTheHillV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait

newtype AscendingTheHillV1 = AscendingTheHillV1 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingTheHillV1 :: ActCard AscendingTheHillV1
ascendingTheHillV1 = act (2, A) AscendingTheHillV1 Cards.ascendingTheHillV1 Nothing

instance HasModifiersFor AscendingTheHillV1 where
  getModifiersFor (AscendingTheHillV1 attrs) = do
    modifySelect attrs Anywhere [NonTraitRestrictedModifier Altered CannotPlaceClues]

instance HasAbilities AscendingTheHillV1 where
  getAbilities (AscendingTheHillV1 x) = [mkAbility x 1 $ forced $ Enters #when You "Sentinel Peak"]

instance RunMessage AscendingTheHillV1 where
  runMessage msg a@(AscendingTheHillV1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> AscendingTheHillV1 <$> liftRunMessage msg attrs
