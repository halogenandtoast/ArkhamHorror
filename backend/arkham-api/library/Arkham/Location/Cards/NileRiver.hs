module Arkham.Location.Cards.NileRiver (nileRiver) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype NileRiver = NileRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nileRiver :: LocationCard NileRiver
nileRiver = location NileRiver Cards.nileRiver 2 (PerPlayer 1)

instance HasAbilities NileRiver where
  getAbilities (NileRiver a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage NileRiver where
  runMessage msg l@(NileRiver attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withI18n $ chooseOneM iid do
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 1
        countVar 1 $ labeled' "discardRandomCardsFromHand" $ randomDiscard iid (attrs.ability 1)
      pure l
    _ -> NileRiver <$> liftRunMessage msg attrs
