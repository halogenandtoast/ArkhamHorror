module Arkham.Location.Cards.DressingRoom (dressingRoom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DressingRoom = DressingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dressingRoom :: LocationCard DressingRoom
dressingRoom = location DressingRoom Cards.dressingRoom 4 (Static 0)

instance HasAbilities DressingRoom where
  getAbilities (DressingRoom a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (HealableInvestigator (a.ability 1) #horror You))
      $ ActionAbility [] (ActionCost 3)

instance RunMessage DressingRoom where
  runMessage msg l@(DressingRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ok <- canHaveHorrorHealed (attrs.ability 1) iid
      when ok $ healHorror iid (attrs.ability 1) 3
      pure l
    _ -> DressingRoom <$> liftRunMessage msg attrs
