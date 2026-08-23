module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.Kitchen (kitchen) where

import Arkham.Ability
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Kitchen = Kitchen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kitchen :: LocationCard Kitchen
kitchen = symbolLabel $ location Kitchen Cards.kitchen 4 (PerPlayer 1)

instance HasAbilities Kitchen where
  getAbilities (Kitchen a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoveringLastClue #after Anyone (be a)

instance RunMessage Kitchen where
  runMessage msg l@(Kitchen attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \iid -> directHorror iid (attrs.ability 1) 1
      pure l
    _ -> Kitchen <$> liftRunMessage msg attrs
