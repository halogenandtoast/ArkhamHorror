module Arkham.Act.Cards.DawnOfTheFirstDay (dawnOfTheFirstDay) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher

newtype DawnOfTheFirstDay = DawnOfTheFirstDay ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dawnOfTheFirstDay :: ActCard DawnOfTheFirstDay
dawnOfTheFirstDay = act (1, A) DawnOfTheFirstDay Cards.dawnOfTheFirstDay Nothing

instance HasModifiersFor DawnOfTheFirstDay where
  getModifiersFor (DawnOfTheFirstDay attrs) =
    modifySelect attrs Anyone [CannotBeDamaged, CannotBeDefeated]

instance RunMessage DawnOfTheFirstDay where
  runMessage msg a@(DawnOfTheFirstDay attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> DawnOfTheFirstDay <$> liftRunMessage msg attrs
