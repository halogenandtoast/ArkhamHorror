module Arkham.Location.Cards.LostMemories (lostMemories) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (lostMemories)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype LostMemories = LostMemories LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostMemories :: LocationCard LostMemories
lostMemories = location LostMemories Cards.lostMemories 2 (PerPlayer 1)

instance HasAbilities LostMemories where
  getAbilities (LostMemories a) =
    extendRevealed1 a
      $ restricted a 1 (youExist InvestigatorWithAnyActionsRemaining)
      $ forced
      $ RevealLocation #after You (be a)

instance RunMessage LostMemories where
  runMessage msg l@(LostMemories attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      actionRemainingCount <- field InvestigatorRemainingActions iid
      assignHorror iid (attrs.ability 1) actionRemainingCount
      pure l
    _ -> LostMemories <$> liftRunMessage msg attrs
