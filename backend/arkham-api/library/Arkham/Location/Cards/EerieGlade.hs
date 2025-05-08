module Arkham.Location.Cards.EerieGlade (eerieGlade) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (eerieGlade)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype EerieGlade = EerieGlade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eerieGlade :: LocationCard EerieGlade
eerieGlade = location EerieGlade Cards.eerieGlade 4 (PerPlayer 1)

instance HasAbilities EerieGlade where
  getAbilities (EerieGlade a) =
    extendRevealed1 a
      $ restricted a 1 (youExist InvestigatorWithAnyActionsRemaining)
      $ forced
      $ RevealLocation #after You (be a)

instance RunMessage EerieGlade where
  runMessage msg l@(EerieGlade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      actionRemainingCount <- field InvestigatorRemainingActions iid
      discardTopOfDeck iid (attrs.ability 1) (actionRemainingCount * 2)
      pure l
    _ -> EerieGlade <$> liftRunMessage msg attrs
