module Arkham.Location.Cards.ReturnToCongregationalChurch (returnToCongregationalChurch) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype ReturnToCongregationalChurch = ReturnToCongregationalChurch LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToCongregationalChurch :: LocationCard ReturnToCongregationalChurch
returnToCongregationalChurch =
  locationWith
    ReturnToCongregationalChurch
    Cards.returnToCongregationalChurch
    1
    (PerPlayer 1)
    (labelL .~ "congregationalChurch")

instance HasAbilities ReturnToCongregationalChurch where
  getAbilities (ReturnToCongregationalChurch a) =
    withDrawCardUnderneathAction a
      <> extendRevealed1 a (mkAbility a 1 $ forced $ RevealLocation #after You (be a))

instance RunMessage ReturnToCongregationalChurch where
  runMessage msg l@(ReturnToCongregationalChurch attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findAndDrawEncounterCard iid Treacheries.kidnapped
      pure l
    _ -> ReturnToCongregationalChurch <$> liftRunMessage msg attrs
